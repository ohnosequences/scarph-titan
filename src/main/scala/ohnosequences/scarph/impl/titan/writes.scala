package ohnosequences.scarph.impl.titan

import org.apache.tinkerpop.gremlin.structure.{ Property => TProperty }
import com.thinkaurelius.titan.core.{
  TitanTransaction => TGraph,
  TitanVertex      => TVertex,
  TitanEdge        => TEdge,
  TitanElement     => TElement
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
        v := graph.addVertex(v.label)
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
    }

  implicit def titanCanSetProperties[E <: AnyGraphElement, P <: AnyProperty { type Source = E }]:
        CanSetProperties[E, TElement, P] =
    new CanSetProperties[E, TElement, P] {

      def setProperty(
        e: E := TElement,
        p: P,
        v: P#Target#Val
      ): E := TElement = {
        e.value.property[P#Target#Val](p.label, v)
        e
      }

    }

}
