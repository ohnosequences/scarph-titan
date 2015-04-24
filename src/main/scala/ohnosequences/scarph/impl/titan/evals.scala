package ohnosequences.scarph.impl.titan

case object evals {

  import implementations._

  import ohnosequences.{ scarph => s }
  import s.objects._, s.morphisms._, s.evals.DefaultEvals

  import com.thinkaurelius.titan.{ core => titan }
  import titan.{TitanVertex, TitanEdge, TitanElement, TitanProperty}

  trait DefaultTitanEvals extends DefaultEvals {

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
