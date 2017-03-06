package ohnosequences.scarph.impl.titan

case object types {

  import org.apache.tinkerpop.gremlin.{ structure => tinkerpop }
  import com.thinkaurelius.titan.{ core => titan }
  import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }
  import ohnosequences.scarph._, impl._

  sealed trait AnyTitanType

  case object TitanZero extends AnyTitanType
  type TitanZero = TitanZero.type

  final case class TitanUnit(val graph: titan.TitanGraph) extends AnyTitanType


  sealed trait AnyDuplet extends AnyTitanType {

    type Left <: AnyTitanType
    val  left: Left

    type Right <: AnyTitanType
    val  right: Right
  }

  final case class Duplet[L <: AnyTitanType, R <: AnyTitanType](val left: L, val right: R) extends AnyDuplet {

    type Left = L
    type Right = R
  }

  sealed trait AnyContainer extends AnyTitanType {

    type Inside
    type Values = Iterable[Inside]
    val  values: Values

    def map[T](f: Inside => T): Container[T] = Container(values.map(f): Iterable[T])
    def flatMap[T](f: Inside => Iterable[T]): Container[T] = Container(values.flatMap(f): Iterable[T])
    def filter(f: Inside => Boolean): Container[Inside] = Container(values.filter(f))
  }

  final case class Container[T](val values: Iterable[T]) extends AnyContainer { type Inside = T }

  final type TitanVertices = Container[titan.TitanVertex]
  final type TitanEdges    = Container[titan.TitanEdge]

}
