package ohnosequences.scarph.impl.titan

case object evals {

  import types._, implementations._, predicates._

  import ohnosequences.{ scarph => s }
  import s.objects._, s.morphisms._, s.evals._

  import com.thinkaurelius.titan.{ core => titan }
  import com.tinkerpop.blueprints
  import com.tinkerpop.blueprints.Direction


  trait DefaultTitanEvals extends TitanRewriteRules {}

  trait TitanRewriteRules extends SpecificTitanEvals {}

  trait SpecificTitanEvals extends DerivedTitanEvals {

    implicit final def eval_quantify_[I, M <: AnyGraphMorphism { type Out = P#Element }, P <: AnyPredicate](
      implicit eval_previous: Eval[I, M, TitanQueries]
    ):  Eval[I, M >=> quantify[P], TitanQueries] =
    new Eval[I, M >=> quantify[P], TitanQueries] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := eval_previous(morph.first)(input).value.map{ addConditions(morph.second.predicate, _) }
      }

      def present(morph: InMorph): String = morph.label
    }

    implicit final def eval_coerce_Edges[
      P <: AnyPredicate { type Element <: AnyEdge }
    ]:  Eval[TitanQueries, coerce[P], TitanEdges] =
    new Eval[TitanQueries, coerce[P], TitanEdges] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := input.value.flatMap{ _.edges.asTitanEdges }
      }

      def present(morph: InMorph): String = morph.label
    }
  }

  trait DerivedTitanEvals extends LowPriorityEvals {

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

    implicit final def mergeImpl[E <: titan.TitanElement]:
      TitanMergeImpl[E] =
      TitanMergeImpl[E]()


    implicit final def zeroImpl[V]:
      TitanZeroImpl[V] =
      TitanZeroImpl[V]()
  }


  trait LowPriorityEvals extends DefaultEvals {

    val graph: titan.TitanGraph


    implicit final def eval_inE[
      E <: AnyEdge
    ]:  Eval[TitanVertices, inE[E], TitanQueries] =
    new Eval[TitanVertices, inE[E], TitanQueries] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := (input.value map {
          _.query
            .labels(morph.edge.label)
            .direction(Direction.IN)
        })
      }

      def present(morph: InMorph): String = morph.label
    }


    implicit final def eval_outE[
      E <: AnyEdge
    ]:  Eval[TitanVertices, outE[E], TitanQueries] =
    new Eval[TitanVertices, outE[E], TitanQueries] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := (input.value map {
          _.query
            .labels(morph.edge.label)
            .direction(Direction.OUT)
        })
      }

      def present(morph: InMorph): String = morph.label
    }

    implicit final def predicateImpl[P <: AnyPredicate, E <: titan.TitanElement]:
      TitanPredicateImpl[P, E] =
      TitanPredicateImpl[P, E]()

  }

}
