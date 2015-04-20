package ohnosequences.scarph.impl.titan

case object types {

  import com.tinkerpop.blueprints
  import com.thinkaurelius.titan.{ core => titan }
  import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }

  final type Container[T]    = Iterable[T]
  final type JIterable[T]    = java.lang.Iterable[T]
  final type TitanVertices   = Container[titan.TitanVertex]
  final type TitanEdges      = Container[titan.TitanEdge]
  final type TitanGraph      = titan.TitanGraph

  final def zero[T]: Container[T] = Seq()

  implicit final def containerOps[T](cs: Container[T]): ContainerOps[T] = ContainerOps[T](cs)
  case class ContainerOps[T](val cs: Container[T]) extends AnyVal {

    @inline final def asJIterable: JIterable[T] = cs.asJava
  }
  implicit final def jIterableOps[T](ts: JIterable[T]): JIterableOps[T] = JIterableOps[T](ts)
  case class JIterableOps[T](val ts: JIterable[T]) extends AnyVal {

    @inline final def asContainer: Container[T] = ts.asScala
  }

  implicit final def blueprintsVerticesOps(es: JIterable[blueprints.Vertex]):
    BlueprintsVerticesOps =
    BlueprintsVerticesOps(es)
  case class BlueprintsVerticesOps(val es: JIterable[blueprints.Vertex]) extends AnyVal {

    @inline final def asTitanVertices: TitanVertices =
      es.asInstanceOf[JIterable[titan.TitanVertex]].asContainer
  }

  implicit final def blueprintsEdgesOps(es: JIterable[blueprints.Edge]):
    BlueprintsEdgesOps =
    BlueprintsEdgesOps(es)
  case class BlueprintsEdgesOps(val es: JIterable[blueprints.Edge]) extends AnyVal {

    @inline final def asTitanEdges: TitanEdges =
      es.asInstanceOf[JIterable[titan.TitanEdge]].asContainer
  }

  case class TitanBiproduct[L, R](val both: (Container[L], Container[R])) extends AnyVal {

    type Left = L
    type Right = R

    @inline final def left  = both._1
    @inline final def right = both._2

    final def map[X, Y](f: L => X, g: R => Y): TitanBiproduct[X, Y] = TitanBiproduct( (left map f, right map g) )
  }

  case class TitanTensor[L, R](val left: Container[L], val right: Container[R]) {

    type Left = L
    type Right = R
  }
}
