package ohnosequences.scarph.impl.titan

case object implementations {

  import com.thinkaurelius.titan.{ core => titan } //, schema._
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
    implicit def unitVertexImpl[V <: AnyVertex, O <: AnyTitanVals]:
        UnitImpl[V, O, titan.TitanGraph] =
    new UnitImpl[V, O, titan.TitanGraph] {

      def toUnit(o: RawObject): RawUnit = graph
      // TODO: return all vertices of this type
      def fromUnit(u: RawUnit, o: Object): RawObject = ???
    }



    implicit def vertexPropertyImpl[P <: AnyGraphProperty]:
        PropertyImpl[P, TitanVals[titan.TitanVertex], TitanVals[P#Raw]] =
    new PropertyImpl[P, TitanVals[titan.TitanVertex], TitanVals[P#Raw]] {

      def get(e: RawElement, p: Property): RawValue = TitanVals(asJavaIterable(
        e.values.map{ _.getProperty[P#Raw](p.label) }
      ))

      def lookup(r: RawValue, p: Property): RawElement = TitanVals(r.values.map { v =>
        graph.query.has(p.label, v).vertices
      }.asInstanceOf[Container[titan.TitanVertex]])
    }

    implicit def edgePropertyImpl[P <: AnyGraphProperty]:
        PropertyImpl[P, TitanEdges, TitanVals[P#Raw]] =
    new PropertyImpl[P, TitanEdges, TitanVals[P#Raw]] {

      def get(e: RawElement, p: Property): RawValue = TitanVals(asJavaIterable(
        e.values.map{ _.getProperty[P#Raw](p.label) }
      ))

      def lookup(r: RawValue, p: Property): RawElement = TitanVals(
        r.values.flatMap { v =>
          graph.query.has(p.label, v).vertices
        }.asInstanceOf[Container[titan.TitanEdge]]
      )
    }

  }

}
