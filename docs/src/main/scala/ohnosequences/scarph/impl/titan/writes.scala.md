
```scala
package ohnosequences.scarph.impl.titan

import org.apache.tinkerpop.gremlin.structure.{ Property => TProperty }
import com.thinkaurelius.titan.core.{
  TitanGraphTransaction => TGraph,
  TitanVertex           => TVertex,
  TitanEdge             => TEdge,
  TitanElement          => TElement
}
import ohnosequences.scarph._
import ohnosequences.scarph.impl.titan.types._
import ohnosequences.cosas.types._

case object writes {

  implicit def titanCanAddVertices[V <: AnyVertex]:
        CanAddVertices[TGraph, V, TVertex] =
    new CanAddVertices[TGraph, V, TVertex] {

      def addVertex(graph: TGraph)(v: V):
        V := TVertex =
        v := graph.addVertex(org.apache.tinkerpop.gremlin.structure.T.label, v.label: String)

      def removeVertex(graph: TGraph)(v: V := TVertex): TGraph =
        { v.value.remove; graph }

    }

  implicit def titanCanAddEdges[E <: AnyEdge]:
        CanAddEdges[TVertex, E, TEdge, TVertex] =
    new CanAddEdges[TVertex, E, TEdge, TVertex] {

      // NOTE: this doesn't use the graph-transaction explicitly
      def addEdge(e: E)(
        src: E#Source := TVertex,
        tgt: E#Target := TVertex
      ): E := TEdge =
         e := src.value.addEdge(e.label, tgt.value)

      def removeEdge(r: E := TEdge): Unit =
        r.value.remove
    }

  implicit def titanCanSetProperties[
    E <: AnyGraphElement,
    ER <: TElement,
    P <: AnyProperty { type Source = E },
    V <: P#Target#Val
  ]:  CanSetProperties[E, ER, P, V] =
  new CanSetProperties[E, ER, P, V] {

    def setProperty(
      e: E := ER,
      p: P,
      v: V
    ): E := ER = {
      e.value.property[V](p.label, v)
      e
    }
  }

}

```




[main/scala/ohnosequences/scarph/impl/titan/predicates.scala]: predicates.scala.md
[main/scala/ohnosequences/scarph/impl/titan/types.scala]: types.scala.md
[main/scala/ohnosequences/scarph/impl/titan/rewrites.scala]: rewrites.scala.md
[main/scala/ohnosequences/scarph/impl/titan/syntax.scala]: syntax.scala.md
[main/scala/ohnosequences/scarph/impl/titan/evals.scala]: evals.scala.md
[main/scala/ohnosequences/scarph/impl/titan/writes.scala]: writes.scala.md
[main/scala/ohnosequences/scarph/impl/titan/morphisms.scala]: morphisms.scala.md
[main/scala/ohnosequences/scarph/impl/titan/titanSchema.scala]: titanSchema.scala.md
[test/scala/ohnosequences/scarph/titan/schemaTests.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/schemaTests.scala.md
[test/scala/ohnosequences/scarph/titan/SchemaCreation.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/SchemaCreation.scala.md
[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala.md