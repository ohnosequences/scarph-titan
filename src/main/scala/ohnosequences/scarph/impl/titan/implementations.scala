package ohnosequences.scarph.impl.titan

case object implementations {

  import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }

  import com.thinkaurelius.titan.{ core => titan }
  import com.tinkerpop.blueprints
  import com.tinkerpop.blueprints.Direction

  import ohnosequences.{ scarph => s }
  import s.objects._, s.morphisms._, s.implementations._
  import titan.{TitanVertex, TitanEdge, TitanElement, TitanProperty}
  import ohnosequences.scarph.impl.titan.types._

  import predicates._


  trait AnyTGraph extends Any {

    def g: TitanGraph
  }

/*
  case class TitanBiproductImpl[L <: AnyTitanType, R <: AnyTitanType](lzero: L, rzero: R) extends BiproductImpl[TitanDuplet[L, R], L, R] {

    def apply(l: RawLeft, r: RawRight): RawBiproduct = TitanDuplet[L, R](l, r)

    def leftProj(b: RawBiproduct): RawLeft = b.left
    def leftInj(l: RawLeft): RawBiproduct = TitanDuplet(l, Container[R](Seq()))

    def rightProj(b: RawBiproduct): RawRight = b.right
    def rightInj(r: RawRight): RawBiproduct = TitanDuplet(Container[L](Seq()), r)
  }

  case class TitanTensorImpl[L <: AnyTitanType, R <: AnyTitanType]() extends TensorImpl[TitanDuplet[L, R], L, R] {

    def apply(l: RawLeft, r: RawRight): RawTensor = TitanDuplet[L, R](l, r)

    def leftProj(t: RawTensor): RawLeft = t.left
    def rightProj(t: RawTensor): RawRight = t.right
  }

  case class TitanMatchUpImpl[T]() extends MatchUpImpl[Container[T]] {

    def matchUp(l: Raw, r: Raw): Raw =
      l flatMap { x =>
        r.values filter { _ == x }
      }
  }

  case class TitanUnitVertexImpl[V <: AnyVertex](val g: TitanGraph)
    extends AnyTGraph with UnitImpl[V, TitanVertices, TitanGraph] {

    def toUnit(o: RawObject): RawUnit = g

    final def fromUnit(u: RawUnit, o: Object): RawObject =
      Container(g.query.has("label", o.label)
        .vertices.asTitanVertices)
  }

  case class TitanUnitEdgeImpl[E <: AnyEdge](val g: TitanGraph)
    extends AnyTGraph with UnitImpl[E, TitanEdges, TitanGraph] {

    def toUnit(o: RawObject): RawUnit = g

    final def fromUnit(u: RawUnit, o: Object): RawObject =
      Container(g.query.has("label", o.label)
        .edges.asTitanEdges)
  }

  case class TitanUnitValueTypeImpl[V <: AnyValueType](val g: TitanGraph)
    extends AnyTGraph with UnitImpl[V, Container[V#Raw], TitanGraph] {

    def toUnit(o: RawObject): RawUnit = g

    final def fromUnit(u: RawUnit, o: Object): RawObject =
      Container(g.query.has("label", o.label)
        .properties.asScala.map{ p => p.getValue })
  }

  // TODO: predicates are also objects, and this fromUnit would be an index lookup
  // case class TitanUnitPredicateImpl[P <: AnyPredicate, TP <: TitanGraphQuery](val g: TitanGraph) { ... }


  case class TitanPropertyVertexImpl[P <: AnyProperty { type Owner <: AnyVertex }](val g: TitanGraph)
    extends AnyTGraph with PropertyImpl[P, TitanVertices, Container[P#Value#Raw]] {

    final def get(e: RawElement, p: Property): RawValue =
      e map { _.getProperty[P#Value#Raw](p.label) }

    final def lookup(r: RawValue, p: Property): RawElement =
      r flatMap { v =>
        g.query.has(p.label, v)
        .vertices.asTitanVertices
      }
  }

  case class TitanPropertyEdgeImpl[P <: AnyProperty { type Owner <: AnyEdge }](val g: TitanGraph)
    extends AnyTGraph with PropertyImpl[P, TitanEdges, Container[P#Value#Raw]] {

    final def get(e: RawElement, p: Property): RawValue =
      e map { _.getProperty[P#Value#Raw](p.label) }

    final def lookup(r: RawValue, p: Property): RawElement =
      r flatMap { v =>
        g.query.has(p.label, v)
        .edges.asTitanEdges
      }
  }

  case class TitanEdgeImpl(val g: TitanGraph)
    extends AnyTGraph with EdgeImpl[TitanEdges, TitanVertices, TitanVertices] {

    final def source(e: RawEdge): RawSource = e map { _.getVertex(Direction.OUT) }

    final def target(e: RawEdge): RawTarget = e map { _.getVertex(Direction.IN) }
  }

  case class ContainerZeroImpl[T]() extends ZeroImpl[Container[T]] {

    def apply(): Raw = Container[T](Seq())
  }

  case class TitanVertexOutImpl[E <: AnyEdge](val g: TitanGraph)
    extends AnyTGraph with VertexOutImpl[E, TitanVertices, TitanEdges, TitanVertices] {

    final def outE(v: RawVertex, e: Edge): RawOutEdge =
      v flatMap {
        _.query
          .labels(e.label)
          .direction(Direction.OUT)
          .titanEdges.asScala
      }

    final def outV(v: RawVertex, e: Edge): RawOutVertex =
      v flatMap {
        _.query
          .labels(e.label)
          .direction(Direction.OUT)
          .vertexIds.asScala
      }
  }


  case class TitanVertexInImpl[E <: AnyEdge](val g: TitanGraph)
    extends AnyTGraph with VertexInImpl[E, TitanVertices, TitanEdges, TitanVertices] {

    final def inE(v: RawVertex, e: Edge): RawInEdge =
      v flatMap {
        _.query
          .labels(e.label)
          .direction(Direction.IN)
          .titanEdges.asScala
      }

    final def inV(v: RawVertex, e: Edge): RawInVertex =
      v flatMap {
        _.query
          .labels(e.label)
          .direction(Direction.IN)
          .vertexIds.asScala
      }
  }

  // Here RawPredicate is the filtered list of elements
  case class TitanPredicateImpl[P <: AnyPredicate, TE <: TitanElement]()
    extends PredicateImpl[P, Container[TE], Container[TE]] {

    def quantify(e: RawElement, p: Predicate): RawPredicate =
      e filter { evalPredicate(p, _) }

    def coerce(p: RawPredicate): RawElement = p
  }
*/

}
