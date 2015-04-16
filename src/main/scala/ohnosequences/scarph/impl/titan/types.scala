package ohnosequences.scarph.impl.titan

case object types {


  import com.thinkaurelius.titan.{ core => titan } 

  type Container[T] = java.lang.Iterable[T]
  import java.util.Iterator

  trait AnyTitanVals extends Any {

    type Obj
    def values: Container[Obj]
  }

  case class TitanVals[T](val values: Container[T])
    extends AnyVal with AnyTitanVals { type Obj = T }

  type TitanVertices = TitanVals[titan.TitanVertex]
  type TitanEdges = TitanVals[titan.TitanEdge]
  type TitanGraph = titan.TitanGraph

  sealed trait product[L,R] extends Any with AnyTitanVals {

    type Obj = (L,R)
  }

  sealed trait either[L,R] extends Any with AnyTitanVals {

    def isLeft: Boolean
    def isRight: Boolean
  }
  case class left[L,R](val values: Container[L]) extends either[L,R] {

    type Obj = L

    @inline final def isLeft = true
    @inline final def isRight = false
  }
  case class right[L,R](val values: Container[L]) extends either[L,R] {

    type Obj = R

    @inline final def isLeft = false
    @inline final def isRight = true
  }
}