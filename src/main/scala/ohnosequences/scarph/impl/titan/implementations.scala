package ohnosequences.scarph.impl.titan

case object implementations {

  import com.thinkaurelius.titan.{ core => titan } //, schema._
  import com.tinkerpop.blueprints.Direction
  import scala.collection.JavaConversions._
  // import java.lang.Iterable

  import ohnosequences.cosas._, fns._, types._
  // import cosas.ops.typeSets._

  import ohnosequences.{ scarph => s }
  import s.graphTypes._, s.morphisms._, s.implementations._
  // import s.impl.titan.predicates._

  type Container[T] = java.lang.Iterable[T]

  trait AnyTitanVals extends Any {

    type Obj
    def values: Container[Obj]
  }

  case class TitanVals[T](val values: Container[T])
    extends AnyVal with AnyTitanVals { type Obj = T }

  import titan.{TitanVertex, TitanEdge, TitanElement}
  type TitanVertices = TitanVals[titan.TitanVertex]
  type TitanEdges = TitanVals[titan.TitanEdge]
  type TitanGraph = titan.TitanGraph


  trait AnyTGraph extends Any {

    def g: TitanGraph
  }

  case class TitanUnitImpl[E <: AnyGraphElement, TE <: TitanElement](val g: TitanGraph) 
    extends AnyVal with AnyTGraph with UnitImpl[E, TitanVals[TE], TitanGraph]  {

    // TODO a better Unit type here
    final def toUnit(o: RawObject): RawUnit = g

    final def fromUnit(u: RawUnit, o: Object): RawObject = TitanVals(
      g.query.has("label", o.label).vertices
        .asInstanceOf[Container[TE]]
    )
  }

  case class TitanPropertyImpl[P <: AnyGraphProperty, TE <: TitanElement](val g: TitanGraph)
    extends AnyVal with AnyTGraph with PropertyImpl[P, TitanVals[TE], TitanVals[P#Raw]] {

    final def get(e: RawElement, p: Property): RawValue = TitanVals(asJavaIterable(
        e.values.map{ _.getProperty[P#Raw](p.label) }
      ))

    final def lookup(r: RawValue, p: Property): RawElement = TitanVals(
        r.values.flatMap { v =>
          g.query.has(p.label, v).edges
        }.asInstanceOf[Container[TE]]
      )
  }



  case class TitanImpls(val graph: titan.TitanGraph) {
 
    // TODO: should work also for properties and predicates
    implicit def unitVertexImpl[V <: AnyVertex]: TitanUnitImpl[V, TitanVertex] = TitanUnitImpl(graph)
    
    implicit def unitEdgeImpl[E <: AnyEdge]: TitanUnitImpl[E, TitanEdge] = TitanUnitImpl(graph)

    implicit def vertexPropertyImpl[P <: AnyGraphProperty { type Owner <: AnyVertex }]:
      TitanPropertyImpl[P, TitanVertex] =
      TitanPropertyImpl(graph)

    implicit def edgePropertyImpl[P <: AnyGraphProperty { type Owner <: AnyEdge }]:
      TitanPropertyImpl[P, TitanEdge] =
      TitanPropertyImpl(graph)

    implicit def edgeImpl:
        EdgeImpl[TitanEdges, TitanVertices, TitanVertices] =
    new EdgeImpl[TitanEdges, TitanVertices, TitanVertices] {

      def source(e: RawEdge): RawSource = TitanVals(asJavaIterable(
        e.values.map{ _.getVertex(Direction.OUT) }
      ))

      def target(e: RawEdge): RawTarget = TitanVals(asJavaIterable(
        e.values.map{ _.getVertex(Direction.IN) }
      ))
    }

    implicit def zeroImpl[V]:
        ZeroImpl[TitanVals[V]] =
    new ZeroImpl[TitanVals[V]] {

      def apply(): Raw = TitanVals[V](Seq())
    }

    implicit def vertexOutImpl[E <: AnyEdge]:
        VertexOutImpl[E, TitanVertices, TitanEdges, TitanVertices] =
    new VertexOutImpl[E, TitanVertices, TitanEdges, TitanVertices] {

      def outE(v: RawVertex, e: Edge): RawOutEdge = TitanVals(asJavaIterable(
        v.values.flatMap{
          _.query.labels(e.label).direction(Direction.OUT).titanEdges
        }
      ))

      def outV(v: RawVertex, e: Edge): RawOutVertex = TitanVals(asJavaIterable(
        v.values.flatMap{
          _.query.labels(e.label).direction(Direction.OUT).vertexIds
        }
      ))
    }

    implicit def vertexInImpl[E <: AnyEdge]:
        VertexInImpl[E, TitanVertices, TitanEdges, TitanVertices] =
    new VertexInImpl[E, TitanVertices, TitanEdges, TitanVertices] {

      def inE(v: RawVertex, e: Edge): RawInEdge = TitanVals(asJavaIterable(
        v.values.flatMap{
          _.query.labels(e.label).direction(Direction.IN).titanEdges
        }
      ))

      def inV(v: RawVertex, e: Edge): RawInVertex = TitanVals(asJavaIterable(
        v.values.flatMap{
          _.query.labels(e.label).direction(Direction.IN).vertexIds
        }
      ))
    }

  }

}
