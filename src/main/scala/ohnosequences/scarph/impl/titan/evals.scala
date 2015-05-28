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


    def outERaw(edge: AnyEdge)(v: RawSource): RawEdge =
      v flatMap {
        _.query
          .labels(edge.label)
          .direction(Direction.OUT)
          .titanEdges.asScala
      }
    def sourceRaw(edge: AnyEdge)(e: RawEdge): RawSource =
      e map { _.getVertex(Direction.OUT) }


    def inERaw(edge: AnyEdge)(v: RawTarget): RawEdge =
      v flatMap {
        _.query
          .labels(edge.label)
          .direction(Direction.IN)
          .titanEdges.asScala
      }
    def targetRaw(edge: AnyEdge)(e: RawEdge): RawTarget =
      e map { _.getVertex(Direction.IN) }

  }

  case object biproductStructure extends BiproductStructure {

    type RawObject = AnyTitanType
    type RawBiproduct[L <: RawObject, R <: RawObject] = Duplet[L, R]
    type RawZero = TitanZero

    def construct[L <: RawObject, R <: RawObject](l: L, r: R): RawBiproduct[L, R] = Duplet(l, r)
    def leftProjRaw[L <: RawObject, R <: RawObject](t: RawBiproduct[L, R]): L = t.left
    def rightProjRaw[L <: RawObject, R <: RawObject](t: RawBiproduct[L, R]): R = t.right
    def toZeroRaw[X <: RawObject](x: X): RawZero = TitanZero

    implicit def containerZero[X]:
        ZeroFor[Container[X]] =
    new ZeroFor[Container[X]] {

      def zero(o: AnyGraphObject): T = Container[X](Seq())
    }

    implicit def dupletZero[L <: AnyTitanType, R <: AnyTitanType](implicit
      l: ZeroFor[L],
      r: ZeroFor[R]
    ):  ZeroFor[Duplet[L, R]] =
    new ZeroFor[Duplet[L, R]] {

      def zero(o: AnyGraphObject): T = Duplet[L, R](l.zero(o), r.zero(o))
    }


    implicit def containerMerge[X]:
        Mergeable[Container[X]] =
    new Mergeable[Container[X]] {

      def merge(l: T, r: T): T =
        Container( l.values ++ r.values )
    }

    implicit def dupletMerge[X <: AnyTitanType](implicit
      t: Mergeable[X]
    ):  Mergeable[Duplet[X, X]] =
    new Mergeable[Duplet[X, X]] {

      def merge(l: T, r: T): T = Duplet(
        t.merge(l.left, r.left),
        t.merge(l.right, r.right)
      )
    }

  }

  case class vertexPropertyStructure[V](val graph: titan.TitanGraph) extends AnyPropertyStructure {

    type RawObject = AnyTitanType
    type RawElement = TitanVertices
    type RawValue = Container[V]
    type PropertyBound = AnyProperty.withRaw[V] { type Owner <: AnyVertex }

    def getRaw[P <: PropertyBound](p: P)(e: RawElement): RawValue =
      e map { _.getProperty[V](p.label) }

    def lookupRaw[P <: PropertyBound](p: P)(v: RawValue): RawElement =
      v flatMap { x =>
        graph.query.has(p.label, x)
          .vertices.asTitanVertices
      }
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
