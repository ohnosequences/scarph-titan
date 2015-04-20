package ohnosequences.scarph.impl.titan

case object implementations {

  import com.thinkaurelius.titan.{ core => titan }
  import com.tinkerpop.blueprints.Direction

  import ohnosequences.{ scarph => s }
  import s.graphTypes._, s.morphisms._, s.implementations._
  import titan.{TitanVertex, TitanEdge, TitanElement, TitanProperty}
  import ohnosequences.scarph.impl.titan.types._


  trait AnyTGraph extends Any {

    def g: TitanGraph
  }

  case class TitanBiproductImpl[L,R]() extends BiproductImpl[TitanBiproduct[L,R], Container[L],Container[R]] {

    @inline final def apply(l: RawLeft, r: RawRight): RawBiproduct = TitanBiproduct( (l,r) )

    @inline final def leftProj(b: RawBiproduct): RawLeft = b.left
    @inline final def leftInj(l: RawLeft): RawBiproduct = TitanBiproduct( (l, Seq()) )

    @inline final def rightProj(b: RawBiproduct): RawRight = b.right
    @inline final def rightInj(r: RawRight): RawBiproduct = TitanBiproduct( (Seq(), r) )
  }

  case class TitanTensorImpl[L,R]() extends AnyTensorImpl {

    type RawTensor = TitanTensor[L,R]

    @inline final def apply(l: RawLeft, r: RawRight): RawTensor = TitanTensor(l,r)

    type RawLeft = Container[L]
    @inline final def leftProj(t: RawTensor): RawLeft = t.left

    type RawRight = Container[R]
    @inline final def rightProj(t: RawTensor): RawRight = t.right
  }


  case class TitanUnitVertexImpl[V <: AnyVertex](val g: TitanGraph)
    extends AnyVal with AnyTGraph with UnitImpl[V, TitanVertices, TitanGraph] {

    @inline final def toUnit(o: RawObject): RawUnit = g

    final def fromUnit(u: RawUnit, o: Object): RawObject =
      g.query.has("label", o.label)
        .vertices.asTitanVertices
        .asContainer
  }

  case class TitanUnitEdgeImpl[E <: AnyEdge](val g: TitanGraph)
    extends AnyVal with AnyTGraph with UnitImpl[E, TitanEdges, TitanGraph] {

    @inline final def toUnit(o: RawObject): RawUnit = g

    final def fromUnit(u: RawUnit, o: Object): RawObject =
      g.query.has("label", o.label)
        .edges.asTitanEdges
        .asContainer
  }

  case class TitanUnitPropertyImpl[P <: AnyValueType](val g: TitanGraph)
    extends AnyVal with AnyTGraph with UnitImpl[P, TitanProperties, TitanGraph] {

    @inline final def toUnit(o: RawObject): RawUnit = g

    final def fromUnit(u: RawUnit, o: Object): RawObject =
      g.query.has("label", o.label)
        .properties
        .asContainer
  }

  // TODO: predicates are also objects, and this fromUnit would be an index lookup
  // case class TitanUnitPredicateImpl[P <: AnyPredicate, TP <: TitanGraphQuery](val g: TitanGraph) { ... }


  case class TitanPropertyVertexImpl[P <: AnyGraphProperty { type Owner <: AnyVertex }](val g: TitanGraph)
    extends AnyVal with AnyTGraph with PropertyImpl[P, TitanVertices, Container[P#Value#Raw]] {

    final def get(e: RawElement, p: Property): RawValue =
      e map { _.getProperty[P#Value#Raw](p.label) }

    final def lookup(r: RawValue, p: Property): RawElement =
      r flatMap { v =>
        g.query.has(p.label, v)
        .vertices.asTitanVertices
        .asContainer
      }
  }

  case class TitanPropertyEdgeImpl[P <: AnyGraphProperty { type Owner <: AnyEdge }](val g: TitanGraph)
    extends AnyVal with AnyTGraph with PropertyImpl[P, TitanEdges, Container[P#Value#Raw]] {

    final def get(e: RawElement, p: Property): RawValue =
      e map { _.getProperty[P#Value#Raw](p.label) }

    final def lookup(r: RawValue, p: Property): RawElement =
      r flatMap { v =>
        g.query.has(p.label, v)
        .edges.asTitanEdges
        .asContainer
      }
  }

  case class TitanEdgeImpl(val g: TitanGraph)
    extends AnyVal with AnyTGraph with EdgeImpl[TitanEdges, TitanVertices, TitanVertices] {

    final def source(e: RawEdge): RawSource = e map { _.getVertex(Direction.OUT)  }

    final def target(e: RawEdge): RawTarget = e map { _.getVertex(Direction.IN)   }
  }

  case class TitanZeroImpl[T](val g: TitanGraph)
    extends AnyVal with AnyTGraph with ZeroImpl[Container[T]] {

    @inline final def apply(): Raw = zero
  }

  case class TitanVertexOutImpl[E <: AnyEdge](val g: TitanGraph)
    extends AnyVal with AnyTGraph with VertexOutImpl[E, TitanVertices, TitanEdges, TitanVertices] {

    final def outE(v: RawVertex, e: Edge): RawOutEdge =
      v flatMap {
        _.query
          .labels(e.label)
          .direction(Direction.OUT)
          .titanEdges
          .asContainer
      }

    final def outV(v: RawVertex, e: Edge): RawOutVertex =
      v flatMap {
        _.query
          .labels(e.label)
          .direction(Direction.OUT)
          .vertexIds
          .asContainer
      }
  }

  case class TitanVertexInImpl[E <: AnyEdge](val g: TitanGraph)
    extends AnyVal with AnyTGraph with VertexInImpl[E, TitanVertices, TitanEdges, TitanVertices] {

    final def inE(v: RawVertex, e: Edge): RawInEdge =
      v flatMap {
        _.query
          .labels(e.label)
          .direction(Direction.IN)
          .titanEdges
          .asContainer
      }

    final def inV(v: RawVertex, e: Edge): RawInVertex =
      v flatMap {
        _.query
          .labels(e.label)
          .direction(Direction.IN)
          .vertexIds
          .asContainer
      }
  }
}
