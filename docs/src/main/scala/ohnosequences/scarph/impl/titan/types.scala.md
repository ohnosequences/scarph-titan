
```scala
package ohnosequences.scarph.impl.titan

case object types {

  import org.apache.tinkerpop.gremlin.{ structure => tinkerpop }
  import com.thinkaurelius.titan.core
  import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }
  import ohnosequences.scarph._, impl._

  sealed trait AnyTitanType

  case object TitanZero extends AnyTitanType
  type TitanZero = TitanZero.type

  final case class TitanUnit(val graph: core.TitanGraphTransaction) extends AnyTitanType


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

  case object Container {

    def apply[T](vs: T*): Container[T] = Container(vs)
  }

  final type TitanVertices = Container[core.TitanVertex]
  final type TitanEdges    = Container[core.TitanEdge]

}

```




[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala.md
[test/scala/ohnosequences/scarph/titan/schemaTests.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/schemaTests.scala.md
[test/scala/ohnosequences/scarph/titan/SchemaCreation.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/SchemaCreation.scala.md
[main/scala/ohnosequences/scarph/impl/titan/predicates.scala]: predicates.scala.md
[main/scala/ohnosequences/scarph/impl/titan/types.scala]: types.scala.md
[main/scala/ohnosequences/scarph/impl/titan/morphisms.scala]: morphisms.scala.md
[main/scala/ohnosequences/scarph/impl/titan/evals.scala]: evals.scala.md
[main/scala/ohnosequences/scarph/impl/titan/syntax.scala]: syntax.scala.md
[main/scala/ohnosequences/scarph/impl/titan/writes.scala]: writes.scala.md
[main/scala/ohnosequences/scarph/impl/titan/rewrites.scala]: rewrites.scala.md
[main/scala/ohnosequences/scarph/impl/titan/titanSchema.scala]: titanSchema.scala.md