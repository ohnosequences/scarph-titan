package ohnosequences.scarph.impl.titan

case object evals {

  import implementations._

  import ohnosequences.{ scarph => s }
  import s.graphTypes._, s.morphisms._, s.evals.DefaultEvals

  import com.thinkaurelius.titan.{ core => titan }
  import titan.{TitanVertex, TitanEdge, TitanElement}

  trait DefaultTitanEvals extends DefaultEvals {

    val graph: titan.TitanGraph

    // TODO: should work also for properties and predicates
    implicit final def unitVertexImpl[V <: AnyVertex]: TitanUnitImpl[V, TitanVertex] = TitanUnitImpl(graph)

    implicit final def unitEdgeImpl[E <: AnyEdge]: TitanUnitImpl[E, TitanEdge] = TitanUnitImpl(graph)

    implicit final def vertexPropertyImpl[P <: AnyGraphProperty { type Owner <: AnyVertex }]:
      TitanPropertyVertexImpl[P] =
      TitanPropertyVertexImpl(graph)

    implicit final def edgePropertyImpl[P <: AnyGraphProperty { type Owner <: AnyEdge }]:
      TitanPropertyEdgeImpl[P] =
      TitanPropertyEdgeImpl(graph)

    implicit final def edgeImpl:
      TitanEdgeImpl =
      TitanEdgeImpl(graph)

    implicit final def vertexOutImpl[E <: AnyEdge]:
      TitanVertexOutImpl[E] =
      TitanVertexOutImpl[E](graph)

    implicit final def vertexInImpl[E <: AnyEdge]:
      TitanVertexInImpl[E] =
      TitanVertexInImpl[E](graph)

    implicit final def biproductImpl[L,R]:
      TitanBiproductImpl[L,R] =
      TitanBiproductImpl()

    implicit final def tensorImpl[L,R]:
      TitanTensorImpl[L,R] =
      TitanTensorImpl()


    implicit final def zeroImpl[V]:
      TitanZeroImpl[V] =
      TitanZeroImpl[V](graph)
  }
}
