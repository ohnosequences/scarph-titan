/*
package ohnosequences.scarph.impl.titan

/* Here are methods for creating Titan schema from an abstract schema description */
object schema {

  import shapeless._, poly._

  import com.thinkaurelius.titan.core._, Multiplicity._
  import com.thinkaurelius.titan.core.schema._
  import com.tinkerpop.blueprints.Direction
  import scala.reflect._

  import ohnosequences.cosas._, typeSets._, fns._, types._
  import ohnosequences.cosas.ops.typeSets._

  import ohnosequences.{ scarph => s }
  import s.graphTypes._, s.containers._, s.indexes._, s.schemas._


  /* This takes an edge type and returns Titan `Multiplicity` (i.e. edge arities) */
  trait EdgeTypeMultiplicity[ET <: AnyEdge] extends Fn1[ET] with Out[Multiplicity]

  object EdgeTypeMultiplicity extends EdgeTypeMultiplicity_2 {

    implicit def one2one[ET <: AnyEdge {
      type Source <: AnyGraphType { type Container = ExactlyOne }
      type Target <: AnyGraphType { type Container = ExactlyOne }
    }]: EdgeTypeMultiplicity[ET] =
    new EdgeTypeMultiplicity[ET] { def apply(et: In1): Out = Multiplicity.ONE2ONE }
  }

  trait EdgeTypeMultiplicity_2 extends EdgeTypeMultiplicity_3 {

    implicit def one2many[ET <: AnyEdge {
      type Source <: AnyGraphType { type Container = ExactlyOne }
    }]: EdgeTypeMultiplicity[ET] =
    new EdgeTypeMultiplicity[ET] { def apply(et: In1): Out = Multiplicity.ONE2MANY }

    implicit def many2one[ET <: AnyEdge {
      type Target <: AnyGraphType { type Container = ExactlyOne }
    }]: EdgeTypeMultiplicity[ET] =
    new EdgeTypeMultiplicity[ET] { def apply(et: In1): Out = Multiplicity.MANY2ONE }
  }

  trait EdgeTypeMultiplicity_3 {

    implicit def many2many[ET <: AnyEdge]:
        EdgeTypeMultiplicity[ET] =
    new EdgeTypeMultiplicity[ET] { def apply(et: In1): Out = Multiplicity.MULTI }
  }


  /* Following `Poly1` functions create separate Titan schema elements from
     the scarph properties/vertices/edges/indexes. They return functions of
     the `TitanManagement => TitanManagement` type, so that we can iterate them
     later on the schema type-sets.
  */
  object addPropertyKey extends Poly1 {
    implicit def default[P <: AnyGraphProperty](implicit cc: ClassTag[P#Raw]) =
      at[P]{ (prop: P) =>
        { (m: TitanManagement) =>
          val clazz = cc.runtimeClass.asInstanceOf[Class[P#Raw]]
          m.makePropertyKey(prop.label).dataType(clazz).make
        }
      }
  }

  sealed trait AnySchemaCheckError { val msg: String }

  sealed trait AnyPropertyKeyError extends AnySchemaCheckError {
    type Property <: AnyGraphProperty
    val  property: Property
  }

  abstract class PropertyKeyError[P <: AnyGraphProperty](val msg: String)
    extends AnyPropertyKeyError { type Property = P }

  case class PropertyKeyDoesntExist[P <: AnyGraphProperty](val property: P)
    extends PropertyKeyError[P](s"The property key for [${property.label}] doesn't exist")

  case class PropertyKeyHasWrongCardinality[P <: AnyGraphProperty](val property: P)
    extends PropertyKeyError[P](s"The property key for [${property.label}] has wrong cardinality")

  case class PropertyKeyHasWrongType[P <: AnyGraphProperty](val property: P, t1: String, t2: String)
    extends PropertyKeyError[P](s"The property key for [${property.label}] has wrong datatype (should be ${t1}, but it is ${t2})")

  object checkPropertyKey extends Poly1 {
    implicit def check[P <: AnyGraphProperty](implicit cc: scala.reflect.ClassTag[P#Raw]) =
      at[P]{ (prop: P) =>
        { (mgmt: TitanManagement) =>

          if ( ! mgmt.containsRelationType(prop.label) ) Left(PropertyKeyDoesntExist(prop))
          else {
            val pkey: PropertyKey = mgmt.getPropertyKey(prop.label)

            if ( com.thinkaurelius.titan.core.Cardinality.SINGLE != pkey.getCardinality )
              Left(PropertyKeyHasWrongCardinality(prop))
            else if ( cc.runtimeClass.asInstanceOf[Class[P#Raw]] != pkey.getDataType )
              Left(PropertyKeyHasWrongType(prop, cc.runtimeClass.asInstanceOf[Class[P#Raw]].toString, pkey.getDataType.toString))
            else Right(pkey)
          }: Either[AnyPropertyKeyError, PropertyKey]
        }
      }
  }


  object addVertexLabel extends Poly1 {
    implicit def default[V <: AnyVertex] = at[V]{ (v: V) =>
      { (m: TitanManagement) => m.makeVertexLabel(v.label).make }
    }
  }

  sealed trait AnyVertexLabelError extends AnySchemaCheckError {
    type Vertex <: AnyVertex
    val  vertex: Vertex
  }

  abstract class VertexLabelError[V <: AnyVertex](val msg: String)
    extends AnyVertexLabelError { type Vertex = V }

  case class VertexLabelDoesntExist[V <: AnyVertex](val vertex: V)
    extends VertexLabelError[V](s"The vertex label for [${vertex.label}] doesn't exist")

  object checkVertexLabel extends Poly1 {
    implicit def check[V <: AnyVertex] =
      at[V]{ (v: V) =>
        { (mgmt: TitanManagement) =>
          if ( ! mgmt.containsVertexLabel(v.label) ) Left(VertexLabelDoesntExist(v))
          else Right(mgmt.getVertexLabel(v.label))
          : Either[AnyVertexLabelError, VertexLabel]
        }
      }
  }


  object addEdgeLabel extends Poly1 {
    implicit def default[E <: AnyEdge](implicit multi: EdgeTypeMultiplicity[E]) = at[E]{ (e: E) =>
      { (m: TitanManagement) => m.makeEdgeLabel(e.label).multiplicity(multi(e)).make }
    }
  }

  sealed trait AnyEdgeLabelError extends AnySchemaCheckError {
    type Edge <: AnyEdge
    val  edge: Edge
  }

  abstract class EdgeLabelError[E <: AnyEdge](val msg: String)
    extends AnyEdgeLabelError { type Edge = E }

  case class EdgeLabelDoesntExist[E <: AnyEdge](val edge: E)
    extends EdgeLabelError[E](s"The edge label for [${edge.label}] doesn't exist")

  case class EdgeLabelHasWrongArity[E <: AnyEdge](val edge: E, multi1: Multiplicity, multi2: Multiplicity)
    extends EdgeLabelError[E](s"The edge label for [${edge.label}] has wrong arity: ${multi1} (extected ${multi2})")

  object checkEdgeLabel extends Poly1 {
    implicit def check[E <: AnyEdge](implicit multi: EdgeTypeMultiplicity[E]) =
      at[E]{ (e: E) =>
        { (mgmt: TitanManagement) =>
          if ( ! mgmt.containsEdgeLabel(e.label) ) Left(EdgeLabelDoesntExist(e))
          else {
            val edge = mgmt.getEdgeLabel(e.label)

            if ( edge.getMultiplicity != multi(e) )
              Left(EdgeLabelHasWrongArity(e, edge.getMultiplicity, multi(e)))
            else Right(mgmt.getEdgeLabel(e.label))
          }: Either[AnyEdgeLabelError, EdgeLabel]
        }
      }
  }


  object propertyLabel extends Poly1 {
    implicit def default[P <: AnyGraphProperty] = at[P]{ _.label }
  }


  object addIndex extends Poly1 {
    implicit def localIx[Ix <: AnyLocalEdgeIndex]
      (implicit propLabels: MapToList[propertyLabel.type, Ix#Properties] with InContainer[String]) =
      at[Ix]{ (ix: Ix) => { (m: TitanManagement) =>
          val direction: Direction = (ix.indexType: AnyLocalIndexType) match {
            case OnlySourceCentric => Direction.OUT
            case OnlyTargetCentric => Direction.IN
            case BothEndsCentric   => Direction.BOTH
          }
          val lbl: EdgeLabel = m.getEdgeLabel(ix.indexedType.label)
          val props: List[PropertyKey] = propLabels(ix.properties).map{ m.getPropertyKey(_) }

          m.buildEdgeIndex(lbl, ix.label, direction, props: _*) : TitanIndex
        }
      }

    private def setUniqueness[Ix <: AnyCompositeIndex](ix: Ix, builder: TitanManagement.IndexBuilder):
      TitanManagement.IndexBuilder = if (ix.uniqueness.bool) builder.unique else builder

    implicit def vertexIx[Ix <: AnyCompositeIndex { type IndexedType <: AnyVertex }]
      (implicit propLabels: MapToList[propertyLabel.type, Ix#Properties] with InContainer[String]) =
      at[Ix]{ (ix: Ix) => { (mgmt: TitanManagement) =>

          val builder = propLabels(ix.properties)
            .foldLeft(mgmt.buildIndex(ix.label, classOf[com.tinkerpop.blueprints.Vertex])){
              (builder, lbl) => builder.addKey(mgmt.getPropertyKey(lbl))
            }

          val elemLabel = mgmt.getVertexLabel(ix.indexedType.label)

          setUniqueness(ix, builder).buildCompositeIndex : TitanIndex
        }
      }

    implicit def edgeIx[Ix <: AnyCompositeIndex { type IndexedType <: AnyEdge }]
      (implicit propLabels: MapToList[propertyLabel.type, Ix#Properties] with InContainer[String]) =
      at[Ix]{ (ix: Ix) => { (mgmt: TitanManagement) =>

          val builder = propLabels(ix.properties)
            .foldLeft(mgmt.buildIndex(ix.label, classOf[com.tinkerpop.blueprints.Edge])){
              (builder, lbl) => builder.addKey(mgmt.getPropertyKey(lbl))
            }

          val elemLabel = mgmt.getEdgeLabel(ix.indexedType.label)

          setUniqueness(ix, builder).buildCompositeIndex : TitanIndex
        }
      }
  }

  sealed trait AnyIndexError extends AnySchemaCheckError {
    type Index <: AnyIndex
    val  index: Index
  }

  abstract class IndexError[V <: AnyIndex](val msg: String)
    extends AnyIndexError { type Index = V }

  case class IndexDoesntExist[V <: AnyIndex](val index: V)
    extends IndexError[V](s"The index [${index.label}] doesn't exist")

  case class GraphIndexHasWrongProperties[V <: AnyIndex](val index: V, props1: String, props2: String)
    extends IndexError[V](s"The index [${index.label}] is built over a wrong set of properties: ${props1} (should be ${props2})")

  case class GraphIndexIsNotMono[V <: AnyCompositeIndex](val index: V)
    extends IndexError[V](s"The index [${index.label}] uniqueness property is not set")

  case class GraphIndexIsNotComposite[V <: AnyCompositeIndex](val index: V)
    extends IndexError[V](s"The index [${index.label}] has wrong type (should be composite)")

  case class LocalIndexHasWrongDirection[V <: AnyLocalEdgeIndex](val index: V, d1: Direction, d2: Direction)
    extends IndexError[V](s"The local index [${index.label}] has wrong direction: ${d1} (expected ${d2})")

  object checkIndex extends Poly1 {
    implicit def compositeIndex[Ix <: AnyCompositeIndex]
      (implicit propLabels: MapToList[propertyLabel.type, Ix#Properties] with InContainer[String]) =
      at[Ix]{ (ix: Ix) => { (mgmt: TitanManagement) =>

          if ( ! mgmt.containsGraphIndex(ix.label) ) Left(IndexDoesntExist(ix))
          else {

            val index = mgmt.getGraphIndex(ix.label)

            if ( index.isUnique != ix.uniqueness.bool )
              Left(GraphIndexIsNotMono(ix))
            else if ( ! index.isCompositeIndex )
              Left(GraphIndexIsNotComposite(ix))
            else {
              val ixPropertyKeys: Set[PropertyKey] =
                propLabels(ix.properties).map{ mgmt.getPropertyKey(_) }.toSet

              if ( index.getFieldKeys.toSet != ixPropertyKeys )
                Left(GraphIndexHasWrongProperties(ix,
                  index.getFieldKeys.toSet.mkString("{", ", ", "}"),
                  ixPropertyKeys.mkString("{", ", ", "}")
                ))
              else Right(index)
            }
          }: Either[AnyIndexError, TitanIndex]
        }
      }

    implicit def localIndex[Ix <: AnyLocalEdgeIndex]
      (implicit propLabels: MapToList[propertyLabel.type, Ix#Properties] with InContainer[String]) =
      at[Ix]{ (ix: Ix) => { (mgmt: TitanManagement) =>

          val lbl: EdgeLabel = mgmt.getEdgeLabel(ix.indexedType.label)

          if ( ! mgmt.containsRelationIndex(lbl, ix.label) ) Left(IndexDoesntExist(ix))
          else {

            val index = mgmt.getRelationIndex(lbl, ix.label)

            val direction: Direction = (ix.indexType: AnyLocalIndexType) match {
              case OnlySourceCentric => Direction.OUT
              case OnlyTargetCentric => Direction.IN
              case BothEndsCentric   => Direction.BOTH
            }

            if ( index.getDirection != direction )
              Left(LocalIndexHasWrongDirection(ix, index.getDirection, direction))
            else Right(index)
          }: Either[AnyIndexError, TitanIndex]
        }
      }

  }


  implicit def titanGraphOps[S <: AnyGraphSchema](g: S := TitanGraph)(implicit sch: S):
    TitanGraphOps[S] =
    TitanGraphOps[S](g)(sch)

  case class TitanGraphOps[S <: AnyGraphSchema](g: S := TitanGraph)(sch: S) {

    def createSchema(implicit
      propertiesMapper: MapToList[addPropertyKey.type, S#Properties] with
                        InContainer[TitanManagement => PropertyKey],
      edgeTypesMapper: MapToList[addEdgeLabel.type, S#Edges] with
                       InContainer[TitanManagement => EdgeLabel],
      vertexTypesMapper: MapToList[addVertexLabel.type, S#Vertices] with
                         InContainer[TitanManagement => VertexLabel],
      indexMapper: MapToList[addIndex.type, S#Indexes] with
                   InContainer[TitanManagement => TitanIndex]
    ) = {
      /* We want this to happen all in _one_ transaction */
      val mgmt = g.value.getManagementSystem

      propertiesMapper(sch.properties).map{ _.apply(mgmt) }
      edgeTypesMapper(sch.edges).map{ _.apply(mgmt) }
      vertexTypesMapper(sch.vertices).map{ _.apply(mgmt) }
      indexMapper(sch.indexes).map{ _.apply(mgmt) }

      mgmt.commit
    }

    def checkSchema(implicit
      propertiesMapper: MapToList[checkPropertyKey.type, S#Properties] with
                        InContainer[TitanManagement => Either[AnyPropertyKeyError, PropertyKey]],
      // edgeTypesMapper: MapToList[addEdgeLabel.type, S#Edges] with
      //                  InContainer[TitanManagement => EdgeLabel],
      vertexTypesMapper: MapToList[checkVertexLabel.type, S#Vertices] with
                         InContainer[TitanManagement => Either[AnyVertexLabelError, VertexLabel]],
      indexMapper: MapToList[checkIndex.type, S#Indexes] with
                   InContainer[TitanManagement => Either[AnyIndexError, TitanIndex]]
    ): List[AnySchemaCheckError] = {
      /* We want this to happen all in _one_ transaction */
      val mgmt = g.value.getManagementSystem

      val pErrs = propertiesMapper(sch.properties).map{ _.apply(mgmt) }.flatMap(_.left.toOption)
      // edgeTypesMapper(sch.edges).map{ _.apply(mgmt) }
      val vErrs = vertexTypesMapper(sch.vertices).map{ _.apply(mgmt) }.flatMap(_.left.toOption)
      val iErrs = indexMapper(sch.indexes).map{ _.apply(mgmt) }.flatMap(_.left.toOption)

      mgmt.commit

      pErrs ++ vErrs ++ iErrs
    }
  }

}
*/
