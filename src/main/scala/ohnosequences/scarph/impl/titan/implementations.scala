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

  case class TitanImpls(val graph: titan.TitanGraph) {

    type Container[T] = java.lang.Iterable[T]

    trait AnyTitanVals {

      type Obj
      val values: Container[Obj]
    }

    case class TitanVals[T](val values: Container[T])
      extends AnyTitanVals { type Obj = T }

    type TitanVertices = TitanVals[titan.TitanVertex]
    type TitanEdges = TitanVals[titan.TitanEdge]

    // TODO: should work also for properties and predicates
    implicit def unitVertexImpl[V <: AnyVertex]:
        UnitImpl[V, TitanVertices, titan.TitanGraph] =
    new UnitImpl[V, TitanVertices, titan.TitanGraph] {

      def toUnit(o: RawObject): RawUnit = graph

      def fromUnit(u: RawUnit, o: Object): RawObject = TitanVals(
        graph.query.has("label", o.label).vertices
          .asInstanceOf[Container[titan.TitanVertex]]
      )
    }

    implicit def unitEdgeImpl[E <: AnyEdge]:
        UnitImpl[E, TitanEdges, titan.TitanGraph] =
    new UnitImpl[E, TitanEdges, titan.TitanGraph] {

      def toUnit(o: RawObject): RawUnit = graph

      def fromUnit(u: RawUnit, o: Object): RawObject = TitanVals(
        graph.query.has("label", o.label).edges
          .asInstanceOf[Container[titan.TitanEdge]]
      )
    }



    implicit def vertexPropertyImpl[P <: AnyGraphProperty]:
        PropertyImpl[P, TitanVals[titan.TitanVertex], TitanVals[P#Raw]] =
    new PropertyImpl[P, TitanVals[titan.TitanVertex], TitanVals[P#Raw]] {

      def get(e: RawElement, p: Property): RawValue = TitanVals(asJavaIterable(
        e.values.map{ _.getProperty[P#Raw](p.label) }
      ))

      def lookup(r: RawValue, p: Property): RawElement = TitanVals(
        r.values.map { v =>
          graph.query.has(p.label, v).vertices
        }.asInstanceOf[Container[titan.TitanVertex]]
      )
    }

    implicit def edgePropertyImpl[P <: AnyGraphProperty]:
        PropertyImpl[P, TitanEdges, TitanVals[P#Raw]] =
    new PropertyImpl[P, TitanEdges, TitanVals[P#Raw]] {

      def get(e: RawElement, p: Property): RawValue = TitanVals(asJavaIterable(
        e.values.map{ _.getProperty[P#Raw](p.label) }
      ))

      def lookup(r: RawValue, p: Property): RawElement = TitanVals(
        r.values.flatMap { v =>
          graph.query.has(p.label, v).edges
        }.asInstanceOf[Container[titan.TitanEdge]]
      )
    }

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
