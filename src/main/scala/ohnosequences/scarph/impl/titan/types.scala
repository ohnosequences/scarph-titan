package ohnosequences.scarph.impl.titan

case object types {

  import com.tinkerpop.blueprints
  import com.thinkaurelius.titan.{ core => titan }
  import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }

  sealed trait AnyTitanType {

    //val zero: this.type*/
  }

  trait AnyDuplet extends AnyTitanType {

    type Left <: AnyTitanType
    val  left: Left

    type Right <: AnyTitanType
    val  right: Right
  }

  case class Duplet[L <: AnyTitanType, R <: AnyTitanType](val left: L, val right: R) extends AnyDuplet {

    type Left = L
    type Right = R

    //lazy val zero = Duplet(left.zero, right.zero)*/
  }

  trait AnyContainer extends AnyTitanType {

    type Inside
    type Values = Iterable[Inside]
    val  values: Values

    def map[T](f: Inside => T): Container[T] = Container(values.map(f))
    def flatMap[T](f: Inside => Iterable[T]): Container[T] = Container(values.flatMap(f))
    def filter(f: Inside => Boolean): Container[Inside] = Container(values.filter(f))
  }

  case class Container[T](val values: Iterable[T]) extends AnyContainer {
    type Inside = T

    //lazy val zero = Container[T](Seq())*/
  }


  final type TitanVertices   = Container[titan.TitanVertex]
  final type TitanEdges      = Container[titan.TitanEdge]
  final type TitanQueries    = Container[blueprints.Query]

  final type JIterable[T]    = java.lang.Iterable[T]
  final type TitanGraph      = titan.TitanGraph

  trait ZeroFor[TT <: AnyTitanType] {

    val zero: TT
  }

  implicit def containerZero[T]:
      ZeroFor[Container[T]] =
  new ZeroFor[Container[T]] {

    val zero = Container[T](Seq())
  }

  implicit def containerDuplet[L <: AnyTitanType, R <: AnyTitanType](implicit
    l: ZeroFor[L],
    r: ZeroFor[R]
  ):  ZeroFor[Duplet[L, R]] =
  new ZeroFor[Duplet[L, R]] {

    val zero = Duplet[L, R](l.zero, r.zero)
  }

/*
  trait Mappable[T <: AnyTitanType] {

    type In = T
    type F
    type Out <: AnyTitanType
    def map(in: In, f: F): Out
  }

  implicit def containerMappable[X, Y]:
      Mappable[Container[X]] =
  new Mappable[Container[X]] {

    type F = X => Y
    type Out = Container[Y]

    def map(in: In, f: F): Out = Container(in.values.map(f))
  }

  implicit def DupletMappable[LX, LY, RX, RY]:
      Mappable[Duplet[LX, RX] =
  new Mappable[Duplet[LX, RX] {

    type F = (LX => LY, RX => RY)
    type Out = Duplet[LY, RY]

    def map(in: In, f: F): Out = Duplet(in.left)
  }
*/

//  case class TitanVertices(val values: Container[titan.TitanVertex])
//    extends AnyTitanObject { type Inside = titan.TitanVertex }
//
//  case class TitanEdges(val values: Container[titan.TitanEdge])
//    extends AnyTitanObject { type Inside = titan.TitanEdge }
//
//  case class TitanQueries(val values: Container[blueprints.Query])
//    extends AnyTitanObject { type Inside = blueprints.Query }


//  final def titanZero[T]: Container[T] = Seq()

//  implicit final def containerOps[T](cs: Container[T]): ContainerOps[T] = ContainerOps[T](cs)
//  case class ContainerOps[T](val cs: Container[T]) extends AnyVal {
//
//    @inline final def asJIterable: JIterable[T] = cs.values.asJava
//  }

  implicit final def jIterableOps[T](ts: JIterable[T]): JIterableOps[T] = JIterableOps[T](ts)
  case class JIterableOps[T](val ts: JIterable[T]) extends AnyVal {

    @inline final def asContainer: Container[T] = Container[T](ts.asScala)
  }

  implicit final def blueprintsVerticesOps(es: JIterable[blueprints.Vertex]):
    BlueprintsVerticesOps =
    BlueprintsVerticesOps(es)
  case class BlueprintsVerticesOps(val es: JIterable[blueprints.Vertex]) extends AnyVal {

    @inline final def asTitanVertices: Iterable[titan.TitanVertex] =
      es.asInstanceOf[JIterable[titan.TitanVertex]].asScala //.asContainer
  }

  implicit final def blueprintsEdgesOps(es: JIterable[blueprints.Edge]):
    BlueprintsEdgesOps =
    BlueprintsEdgesOps(es)
  case class BlueprintsEdgesOps(val es: JIterable[blueprints.Edge]) extends AnyVal {

    @inline final def asTitanEdges: Iterable[titan.TitanEdge] =
      es.asInstanceOf[JIterable[titan.TitanEdge]].asScala //.asContainer
  }

}
