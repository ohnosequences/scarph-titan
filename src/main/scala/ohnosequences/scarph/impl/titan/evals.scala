package ohnosequences.scarph.impl.titan

case object evals {

  import types._, implementations._, predicates._

  import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }

  import ohnosequences.{ scarph => s }
  import s.objects._, s.morphisms._, s.evals._, s.rewrites._

  import com.thinkaurelius.titan.{ core => titan }
  import com.tinkerpop.blueprints
  import com.tinkerpop.blueprints.Direction


  case object categoryStructure extends CategoryStructure {

    type RawObject = AnyTitanType
  }

  case class tensorStructure(val graph: titan.TitanGraph) extends TensorStructure {

    type RawObject = AnyTitanType
    type RawTensor[L <: RawObject, R <: RawObject] = Duplet[L, R]
    type RawUnit = titan.TitanGraph

    def construct[L <: RawObject, R <: RawObject](l: L, r: R): RawTensor[L, R] = Duplet(l, r)
    def leftProjRaw[L <: RawObject, R <: RawObject](t: RawTensor[L, R]): L = t.left
    def rightProjRaw[L <: RawObject, R <: RawObject](t: RawTensor[L, R]): R = t.right
    def toUnitRaw[X <: RawObject](x: X): RawUnit = graph

    implicit def titanUnitToVertices:
        FromUnit[TitanUnit, TitanVertices] =
    new FromUnit[TitanUnit, TitanVertices] {

      def fromUnit(u: U, e: AnyGraphObject): T =
        Container(graph.query.has("label", e.label)
          .vertices.asTitanVertices)
    }

    implicit def titanUnitToEdges:
        FromUnit[TitanUnit, TitanEdges] =
    new FromUnit[TitanUnit, TitanEdges] {

      def fromUnit(u: U, e: AnyGraphObject): T =
        Container(graph.query.has("label", e.label)
          .edges.asTitanEdges)
    }


    implicit def containerMatch[X]:
        Matchable[Container[X]] =
    new Matchable[Container[X]] {

      def matchUp(l: T, r: T): T =
        Container(
          l.values.flatMap { x =>
            r.values.filter { _ == x }
          }
        )
    }

    implicit def dupletMatch[X <: AnyTitanType](implicit
      t: Matchable[X]
    ):  Matchable[Duplet[X, X]] =
    new Matchable[Duplet[X, X]] {

      def matchUp(l: T, r: T): T = Duplet(
        t.matchUp(l.left, r.left),
        t.matchUp(l.right, r.right)
      )
    }

  }

  case object graphStructure extends GraphStructure {

    type RawEdge = TitanEdges
    type RawSource = TitanVertices
    type RawTarget = TitanVertices

    def outVRaw(edge: AnyEdge)(v: RawSource): RawTarget =
      Container(
        v.values flatMap {
          _.query
            .labels(edge.label)
            .direction(Direction.OUT)
            .vertexIds.asScala
        }
      )

    def inVRaw(edge: AnyEdge)(v: RawTarget): RawSource =
      Container(
        v.values flatMap {
          _.query
            .labels(edge.label)
            .direction(Direction.IN)
            .vertexIds.asScala
        }
      )

  }

/*
  trait DefaultTitanEvals extends TitanRewriteRules {}

  trait TitanRewriteRules extends AnyRewriteStrategy with SpecificTitanEvals {}

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
*/

/*
    // X → X ⊕ X
    implicit final def eval_fork[
      I, T <: AnyGraphObject, O
    ](implicit
      outBip: BiproductImpl[O, I, I]
    ):  Eval[I, fork[T], O] =
    new Eval[I, fork[T], O] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := outBip(input.value, input.value)
      }

      def present(morph: InMorph): String = morph.label
    }

    // X ⊕ X → X
    implicit final def eval_merge[
      I, T <: AnyGraphObject, O
    ](implicit
      bipImpl: BiproductImpl[I, O, O],
      mergeImpl: MergeImpl[O]
    ):  Eval[I, merge[T], O] =
    new Eval[I, merge[T], O] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := mergeImpl.merge(bipImpl.leftProj(input.value), bipImpl.rightProj(input.value))
      }

      def present(morph: InMorph): String = morph.label
    }

    // L → L ⊕ R
    implicit final def eval_leftInj[
      L <: AnyGraphObject, R <: AnyGraphObject,
      I, OR, O
    ](implicit
      outBip: BiproductImpl[O, I, OR]
    ):  Eval[I, leftInj[L ⊕ R], O] =
    new Eval[I, leftInj[L ⊕ R], O] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := outBip.leftInj(input.value)
      }

      def present(morph: InMorph): String = morph.label
    }

    // R → L ⊕ R
    implicit final def eval_rightInj[
      L <: AnyGraphObject, R <: AnyGraphObject,
      OL, OR, O
    ](implicit
      outBip: BiproductImpl[O, OL, OR]
    ):  Eval[OR, rightInj[L ⊕ R], O] =
    new Eval[OR, rightInj[L ⊕ R], O] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := outBip.rightInj(input.value)
      }

      def present(morph: InMorph): String = morph.label
    }

    // L ⊕ R → L
    implicit final def eval_leftProj[
      IL, IR, I,
      L <: AnyGraphObject, R <: AnyGraphObject
    ](implicit
      outBip: BiproductImpl[I, IL, IR]
    ):  Eval[I, leftProj[L ⊕ R], IL] =
    new Eval[I, leftProj[L ⊕ R], IL] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := outBip.leftProj(input.value)
      }

      def present(morph: InMorph): String = morph.label
    }

    // L ⊕ R → R
    implicit final def eval_rightProj[
      IL, IR, I,
      L <: AnyGraphObject, R <: AnyGraphObject
    ](implicit
      outBip: BiproductImpl[I, IL, IR]
    ):  Eval[I, rightProj[L ⊕ R], IR] =
    new Eval[I, rightProj[L ⊕ R], IR] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := outBip.rightProj(input.value)
      }

      def present(morph: InMorph): String = morph.label
    }
*/

/*
    // 0 → X
    implicit final def eval_fromZero[
      T <: AnyTitanType, X <: AnyGraphObject
    ](implicit
      z: ZeroFor[T]
    ):  Eval[T, fromZero[X], T] =
    new Eval[T, fromZero[X], T] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := z.zero
      }

      def present(morph: InMorph): String = morph.label
    }

    // X → 0 (exactly the same)
    implicit final def eval_toZero[
      T <: AnyTitanType, X <: AnyGraphObject
    ](implicit
      z: ZeroFor[T]
    ):  Eval[T, toZero[X], T] =
    new Eval[T, toZero[X], T] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        morph.out := z.zero
      }

      def present(morph: InMorph): String = morph.label
    }
*/

/*
    implicit final def eval_get[
      P <: AnyProperty, RawElem, RawValue
    ](implicit
      propImpl: PropertyImpl[P, RawElem, RawValue]
    ):  Eval[RawElem, get[P], RawValue] =
    new Eval[RawElem, get[P], RawValue] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := propImpl.get(input.value, morph.property)
      }

      def present(morph: InMorph): String = morph.label
    }

    implicit final def eval_lookup[
      P <: AnyProperty, RawElem, RawValue
    ](implicit
      propImpl: PropertyImpl[P, RawElem, RawValue]
    ):  Eval[RawValue, lookup[P], RawElem] =
    new Eval[RawValue, lookup[P], RawElem] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := propImpl.lookup(input.value, morph.property)
      }

      def present(morph: InMorph): String = morph.label
    }


    implicit final def eval_quantify[
      P <: AnyPredicate, RawPred, RawElem
    ](implicit
      predImpl: PredicateImpl[P, RawPred, RawElem]
    ):  Eval[RawElem, quantify[P], RawPred] =
    new Eval[RawElem, quantify[P], RawPred] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := predImpl.quantify(input.value, morph.predicate)
      }

      def present(morph: InMorph): String = morph.label
    }


    implicit final def eval_coerce[
      P <: AnyPredicate, RawPred, RawElem
    ](implicit
      predImpl: PredicateImpl[P, RawPred, RawElem]
    ):  Eval[RawPred, coerce[P], RawElem] =
    new Eval[RawPred, coerce[P], RawElem] {

      def apply(morph: InMorph): OutMorph = { input: Input =>
        (morph.out: InMorph#Out) := predImpl.coerce(input.value)
      }

      def present(morph: InMorph): String = morph.label
    }
    */


/*
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
*/

}
