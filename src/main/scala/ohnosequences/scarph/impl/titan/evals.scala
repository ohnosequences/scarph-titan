package ohnosequences.scarph.impl.titan

case object evals {

  import types._, implementations._, predicates._

  import ohnosequences.{ scarph => s }
  import s.objects._, s.morphisms._, s.evals._

  import com.thinkaurelius.titan.{ core => titan }
  import com.tinkerpop.blueprints


  trait DefaultTitanEvals extends TitanRewriteRules {}

  trait TitanRewriteRules extends SpecificTitanEvals {

    implicit def rewrite_quantify[
      F <: AnyGraphMorphism,
      G <: AnyGraphMorphism { type In = F#Out; type Out = P#Element },
      P <: AnyPredicate
    ]:  Rewrite[(F >=> G) >=> quantify[P], F >=> (G >=> quantify[P])] =
    new Rewrite[(F >=> G) >=> quantify[P], F >=> (G >=> quantify[P])] {

      def apply(morph: InMorph): OutMorph = {
        val f = morph.first.first
        val g = morph.first.second
        val h = morph.second
        f >=> (g >=> h)
      }
    }
  }

  trait SpecificTitanEvals extends DerivedTitanEvals {

    implicit final def eval_quantify[
      P <: AnyPredicate
    ](implicit
      addConditions: ToBlueprintsQuery[P]
    ):  Eval[TitanQueries, quantify[P], TitanQueries] =
    new Eval[TitanQueries, quantify[P], TitanQueries] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := input.value.map{ addConditions(morph.predicate, _) }
      }

      def present(morph: InMorph): String = morph.label
    }

    implicit final def eval_coerce_Edges[
      E <: AnyEdge, P <: AnyPredicate.On[E]
    ]:  Eval[TitanQueries, coerce[P], TitanEdges] =
    new Eval[TitanQueries, coerce[P], TitanEdges] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := input.value.flatMap{ _.edges.asTitanEdges }
      }

      def present(morph: InMorph): String = morph.label
    }

    implicit final def eval_coerce_Vertices[
      E <: AnyVertex, P <: AnyPredicate.On[E]
    ]:  Eval[TitanQueries, coerce[P], TitanVertices] =
    new Eval[TitanQueries, coerce[P], TitanVertices] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := input.value.flatMap{ _.vertices.asTitanVertices }
      }

      def present(morph: InMorph): String = morph.label
    }

  }

  trait DerivedTitanEvals extends DefaultEvals {

    val graph: titan.TitanGraph


    implicit final def tensorImpl[L, R]:
      TitanTensorImpl[L, R] =
      TitanTensorImpl[L, R]()


    implicit final def unitVertexImpl[V <: AnyVertex]:
      TitanUnitVertexImpl[V] =
      TitanUnitVertexImpl[V](graph)

    implicit final def unitEdgeImpl[E <: AnyEdge]:
      TitanUnitEdgeImpl[E] =
      TitanUnitEdgeImpl[E](graph)

    implicit final def unitPropertyImpl[P <: AnyValueType]:
      TitanUnitValueTypeImpl[P] =
      TitanUnitValueTypeImpl[P](graph)


    implicit final def vertexPropertyImpl[P <: AnyProperty { type Owner <: AnyVertex }]:
      TitanPropertyVertexImpl[P] =
      TitanPropertyVertexImpl[P](graph)

    implicit final def edgePropertyImpl[P <: AnyProperty { type Owner <: AnyEdge }]:
      TitanPropertyEdgeImpl[P] =
      TitanPropertyEdgeImpl[P](graph)


    implicit final def edgeImpl:
      TitanEdgeImpl =
      TitanEdgeImpl(graph)


    implicit final def vertexOutImpl[E <: AnyEdge]:
      TitanVertexOutImpl[E] =
      TitanVertexOutImpl[E](graph)

    implicit final def vertexInImpl[E <: AnyEdge]:
      TitanVertexInImpl[E] =
      TitanVertexInImpl[E](graph)


    implicit final def biproductImpl[L, R]:
      TitanBiproductImpl[L, R] =
      TitanBiproductImpl[L, R]()


    implicit final def zeroImpl[V]:
      TitanZeroImpl[V] =
      TitanZeroImpl[V]()
  }
}
