package ohnosequences.scarph.impl.titan

case object evals {

  import types._, predicates._

  import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }

  import ohnosequences.scarph._, impl._

  import com.thinkaurelius.titan.core
  import org.apache.tinkerpop.gremlin.structure.Direction;


  trait TitanCategoryStructure extends DaggerCategory



  trait TitanGraphStructure extends Relations {

    type RawEdge = TitanEdges
    type RawSource = TitanVertices
    type RawTarget = TitanVertices

    def raw_outV(edge: AnyEdge)(v: RawSource): RawTarget =
      Container(
        v.values flatMap {
          _.query
            .labels(edge.label)
            .direction(Direction.OUT)
            .vertices.asScala
        }
      )
    def raw_inV(edge: AnyEdge)(v: RawTarget): RawSource =
      Container(
        v.values flatMap {
          _.query
            .labels(edge.label)
            .direction(Direction.IN)
            .vertices.asScala
        }
      )


    def raw_outE(edge: AnyEdge)(v: RawSource): RawEdge =
      v flatMap {
        _.query
          .labels(edge.label)
          .direction(Direction.OUT)
          .edges.asScala
      }
    def raw_source(edge: AnyEdge)(e: RawEdge): RawSource =
      e map { _.vertex(Direction.OUT) }


    def raw_inE(edge: AnyEdge)(v: RawTarget): RawEdge =
      v flatMap {
        _.query
          .labels(edge.label)
          .direction(Direction.IN)
          .edges.asScala
      }
    def raw_target(edge: AnyEdge)(e: RawEdge): RawTarget =
      e map { _.vertex(Direction.IN) }

  }



  trait TitanGraph {

    val graph: core.TitanGraphTransaction
  }

  trait TitanTensorStructure extends Tensors with TitanGraph {

    type TensorBound = AnyTitanType
    type RawTensor[L <: TensorBound, R <: TensorBound] = Duplet[L, R]
    type RawUnit = TitanUnit

    def raw_tensor[L <: TensorBound, R <: TensorBound](l: L, r: R): RawTensor[L, R] = Duplet(l, r)
    def raw_left[L <: TensorBound, R <: TensorBound](t: RawTensor[L, R]): L = t.left
    def raw_right[L <: TensorBound, R <: TensorBound](t: RawTensor[L, R]): R = t.right
    def raw_toUnit[X <: TensorBound](x: X): RawUnit = TitanUnit(graph)

    implicit final def eval_fromUnitV[
      T <: AnyVertex
    ]:  Eval[RawUnit, fromUnit[T], TitanVertices] =
    new Eval[RawUnit, fromUnit[T], TitanVertices] {

      def raw_apply(morph: InMorph): RawInput => RawOutput = { raw_input: RawInput =>

        Container(graph.query.has("label", morph.obj.label)
          .vertices.asScala)
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

    implicit final def eval_fromUnitE[
      T <: AnyEdge
    ]:  Eval[RawUnit, fromUnit[T], TitanEdges] =
    new Eval[RawUnit, fromUnit[T], TitanEdges] {

      def raw_apply(morph: InMorph): RawInput => RawOutput = { raw_input: RawInput =>

        Container(graph.query.has("label", morph.obj.label)
          .edges.asScala)
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
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


  trait TitanBiproductStructure extends Biproducts {

    type BiproductBound = AnyTitanType
    type RawBiproduct[L <: BiproductBound, R <: BiproductBound] = Duplet[L, R]
    type RawZero = TitanZero

    def raw_biproduct[L <: BiproductBound, R <: BiproductBound](l: L, r: R): RawBiproduct[L, R] = Duplet(l, r)
    def raw_leftProj[L <: BiproductBound, R <: BiproductBound](t: RawBiproduct[L, R]): L = t.left
    def raw_rightProj[L <: BiproductBound, R <: BiproductBound](t: RawBiproduct[L, R]): R = t.right
    def raw_toZero[X <: BiproductBound](x: X): RawZero = TitanZero

    implicit def verticesZero[V <: AnyVertex]:
        ZeroFor[V, TitanVertices] =
    new ZeroFor[V, TitanVertices] { def zero(o: Obj): T = Container[core.TitanVertex](Iterable()) }

    implicit def edgesZero[E <: AnyEdge]:
        ZeroFor[E, TitanEdges] =
    new ZeroFor[E, TitanEdges] { def zero(o: Obj): T = Container[core.TitanEdge](Iterable()) }

    implicit def valuesZero[VT <: AnyValueType]:
        ZeroFor[VT, Container[VT#Val]] =
    new ZeroFor[VT, Container[VT#Val]] { def zero(o: Obj): T = Container[VT#Val](Iterable()) }

    implicit def predicatesZero[P <: AnyPredicate, E](implicit elem: ZeroFor[P#Element, E]):
        ZeroFor[P, E] =
    new ZeroFor[P, E] { def zero(o: Obj): T = elem.zero(o.element) }


    implicit def containerMerge[X]:
        RawMerge[Container[X]] =
    new RawMerge[Container[X]] {

      def apply(l: T, r: T): T =
        Container( l.values ++ r.values )
    }

    implicit def dupletMerge[X <: AnyTitanType](implicit
      raw_merge: RawMerge[X]
    ):  RawMerge[Duplet[X, X]] =
    new RawMerge[Duplet[X, X]] {

      def apply(l: T, r: T): T = Duplet(
        raw_merge(l.left, r.left),
        raw_merge(l.right, r.right)
      )
    }

  }

  trait TitanPropertyStructure extends TitanGraph {

    implicit def eval_get[E <: core.TitanElement, P <: AnyProperty]:
        Eval[Container[E], get[P], Container[P#Target#Val]] =
    new Eval[Container[E], get[P], Container[P#Target#Val]] {

      def raw_apply(morph: InMorph): RawInput => RawOutput = { elements =>
        elements map { _.property[P#Target#Val](morph.relation.label).value }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }


    implicit def eval_lookupV[
      V,
      P <: AnyProperty {
        type Source <: AnyVertex
        type Target <: AnyValueType { type Val = V }
      }
    ]:
        Eval[Container[V], lookup[P], TitanVertices] =
    new Eval[Container[V], lookup[P], TitanVertices] {

      def raw_apply(morph: InMorph): RawInput => RawOutput = { values =>
        values flatMap { v =>
          graph.query.has(morph.relation.label, v)
            .vertices.asScala
        }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

    implicit def eval_lookupE[
      V,
      P <: AnyProperty {
        type Source <: AnyEdge
        type Target <: AnyValueType { type Val = V }
      }
    ]:
        Eval[Container[V], lookup[P], TitanEdges] =
    new Eval[Container[V], lookup[P], TitanEdges] {

      def raw_apply(morph: InMorph): RawInput => RawOutput = { values =>
        values flatMap { v =>
          graph.query.has(morph.relation.label, v)
            .edges.asScala
        }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

    implicit def eval_lookupE_Alt[
      P <: AnyProperty {
        type Source <: AnyEdge;
      }
    ]
    :   Eval[Container[P#Target#Val], inV[P], TitanEdges] =
    new Eval[Container[P#Target#Val], inV[P], TitanEdges] {

      def raw_apply(morph: InMorph): RawInput => RawOutput = { values =>
        values flatMap { v =>
          graph.query.has(morph.relation.label, v)
            .edges.asScala
        }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }


  }

  trait TitanPredicateStructure {

    implicit final def eval_quantify[
      P <: AnyPredicate, E <: core.TitanElement
    ]:  Eval[Container[E], quantify[P], Container[E]] =
    new Eval[Container[E], quantify[P], Container[E]] {

      def raw_apply(morph: InMorph): RawInput => RawOutput = { elements =>
        elements filter { evalPredicate(morph.predicate, _) }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }


    implicit final def eval_coerce[
      P <: AnyPredicate, E <: core.TitanElement
    ]:  Eval[Container[E], coerce[P], Container[E]] =
    new Eval[Container[E], coerce[P], Container[E]] {

      def raw_apply(morph: InMorph): RawInput => RawOutput = x => x

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

  }

  trait TitanAfterRewriteEvals {
    // import morphisms._
    //
    // implicit final def eval_quantifyOutE[
    //   P <: AnyPredicate { type Element <: AnyEdge }
    // ]:  Eval[TitanVertices, quantifyOutE[P], TitanEdges] =
    // new Eval[TitanVertices, quantifyOutE[P], TitanEdges] {
    //
    //   def raw_apply(morph: InMorph): RawInput => RawOutput = { vertices =>
    //     vertices flatMap { v =>
    //       addConditions(morph.predicate,
    //         v.query
    //         .labels(morph.edge.label)
    //         .direction(Direction.IN)
    //       )
    //       .edges
    //     }
    //   }
    //
    //   def present(morph: InMorph): Seq[String] = Seq(morph.label)
    // }

  }

  // NOTE: these separate "modules" are useful for testing
  case object categoryStructure extends TitanCategoryStructure
  case object graphStructure extends TitanGraphStructure
  case class  tensorStructure(val graph: core.TitanGraphTransaction) extends TitanTensorStructure
  case object biproductStructure extends TitanBiproductStructure
  case class  propertyStructure(val graph: core.TitanGraphTransaction) extends TitanPropertyStructure
  case object predicateStructure extends TitanPredicateStructure

  case class titanScarph(val graph: core.TitanGraphTransaction) extends
    TitanCategoryStructure with
    TitanGraphStructure with
    TitanTensorStructure with
    TitanBiproductStructure with
    TitanPropertyStructure with
    TitanPredicateStructure with
    TitanAfterRewriteEvals

}
