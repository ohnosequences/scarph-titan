package ohnosequences.scarph.impl.titan

case object evals {

  import types._, predicates._

  import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }

  import ohnosequences.{ scarph => s }
  import s.objects._, s.morphisms._, s.evals._

  import com.thinkaurelius.titan.core
  import com.tinkerpop.blueprints
  import com.tinkerpop.blueprints.Direction


  trait TitanCategoryStructure extends CategoryStructure



  trait TitanGraphStructure extends GraphStructure {

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



  trait TitanGraph {

    val graph: core.TitanGraph
  }

  trait TitanTensorStructure extends TensorStructure with TitanGraph {

    type TensorBound = AnyTitanType
    type RawTensor[L <: TensorBound, R <: TensorBound] = Duplet[L, R]
    type RawUnit = core.TitanGraph

    def tensorRaw[L <: TensorBound, R <: TensorBound](l: L, r: R): RawTensor[L, R] = Duplet(l, r)
    def leftRaw[L <: TensorBound, R <: TensorBound](t: RawTensor[L, R]): L = t.left
    def rightRaw[L <: TensorBound, R <: TensorBound](t: RawTensor[L, R]): R = t.right
    def toUnitRaw[X <: TensorBound](x: X): RawUnit = graph

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



  trait TitanBiproductStructure extends BiproductStructure {

    type BiproductBound = AnyTitanType
    type RawBiproduct[L <: BiproductBound, R <: BiproductBound] = Duplet[L, R]
    type RawZero = TitanZero

    def biproductRaw[L <: BiproductBound, R <: BiproductBound](l: L, r: R): RawBiproduct[L, R] = Duplet(l, r)
    def leftProjRaw[L <: BiproductBound, R <: BiproductBound](t: RawBiproduct[L, R]): L = t.left
    def rightProjRaw[L <: BiproductBound, R <: BiproductBound](t: RawBiproduct[L, R]): R = t.right
    def toZeroRaw[X <: BiproductBound](x: X): RawZero = TitanZero

    implicit def containerZero[O <: AnyGraphObject, X]:
        ZeroFor[O, Container[X]] =
    new ZeroFor[O, Container[X]] {

      def zero(o: Obj): T = Container[X](Seq())
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


  trait TitanPropertyStructure extends TitanGraph {

    implicit def eval_get[E <: core.TitanElement, VT, P <: AnyProperty]:
        Eval[Container[E], get[P], Container[VT]] =
    new Eval[Container[E], get[P], Container[VT]] {

      def rawApply(morph: InMorph): InVal => OutVal = { elements =>
        elements map { _.getProperty[VT](morph.property.label) }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }


    implicit def eval_lookupV[VT, P <: AnyProperty.withRaw[VT] { type Owner <: AnyVertex }]:
        Eval[Container[VT], lookup[P], TitanVertices] =
    new Eval[Container[VT], lookup[P], TitanVertices] {

      def rawApply(morph: InMorph): InVal => OutVal = { values =>
        values flatMap { v =>
          graph.query.has(morph.property.label, v)
            .vertices.asTitanVertices
        }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

    implicit def eval_lookupE[VT, P <: AnyProperty.withRaw[VT] { type Owner <: AnyEdge }]:
        Eval[Container[VT], lookup[P], TitanEdges] =
    new Eval[Container[VT], lookup[P], TitanEdges] {

      def rawApply(morph: InMorph): InVal => OutVal = { values =>
        values flatMap { v =>
          graph.query.has(morph.property.label, v)
            .edges.asTitanEdges
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

      def rawApply(morph: InMorph): InVal => OutVal = { elements =>
        elements filter { evalPredicate(morph.predicate, _) }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }


    implicit final def eval_coerce[
      P <: AnyPredicate, E <: core.TitanElement
    ]:  Eval[Container[E], coerce[P], Container[E]] =
    new Eval[Container[E], coerce[P], Container[E]] {

      def rawApply(morph: InMorph): InVal => OutVal = x => x

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

  }

  trait TitanAfterRewriteEvals {
    import morphisms._

    implicit final def eval_quantifyOutE[
      P <: AnyPredicate { type Element <: AnyEdge }
    ]:  Eval[TitanVertices, quantifyOutE[P], TitanEdges] =
    new Eval[TitanVertices, quantifyOutE[P], TitanEdges] {

      def rawApply(morph: InMorph): InVal => OutVal = { vertices =>
        vertices flatMap { v =>
          addConditions(morph.predicate,
            v.query
            .labels(morph.edge.label)
            .direction(Direction.IN)
          )
          .edges.asTitanEdges
        }
      }

      def present(morph: InMorph): Seq[String] = Seq(morph.label)
    }

  }


  case object categoryStructure extends TitanCategoryStructure
  case object graphStructure extends TitanGraphStructure
  case class  tensorStructure(val graph: core.TitanGraph) extends TitanTensorStructure
  case object biproductStructure extends TitanBiproductStructure
  case class  propertyStructure(val graph: core.TitanGraph) extends TitanPropertyStructure
  case object predicateStructure extends TitanPredicateStructure

  case class all(val graph: core.TitanGraph) extends
    TitanCategoryStructure with
    TitanGraphStructure with
    TitanTensorStructure with
    TitanBiproductStructure with
    TitanPropertyStructure with
    TitanPredicateStructure with
    TitanAfterRewriteEvals

}
