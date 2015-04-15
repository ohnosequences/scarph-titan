package ohnosequences.scarph.impl.titan

case object implementations {

  import com.thinkaurelius.titan.{ core => titan } //, schema._
  import scala.collection.JavaConversions._
  import java.lang.{ Iterable => JIterable }

  import ohnosequences.cosas._, fns._, types._
  // import cosas.ops.typeSets._

  import ohnosequences.{ scarph => s }
  import s.graphTypes._, s.morphisms._, s.implementations._
  // import s.impl.titan.predicates._

  trait AnyTitanElement {

    val graph: titan.TitanGraph

    type Obj
    val values: JIterable[Obj]
  }

  implicit def unitImpl[O <: AnyTitanElement]:
      UnitImpl[titan.TitanGraph, O] =
  new UnitImpl[titan.TitanGraph, O] {

    def toUnit(o: Obj): UnitImpl = o.graph
    // TODO: return all graph elements of this type
    def fromUnit(u: UnitImpl): Obj = ???
  }


  trait AnyTitanVertexImpl extends AnyTitanElement {

    type Obj = titan.TitanVertex
  }


  implicit def propertyImpl[P <: AnyGraphProperty]:
      PropertyImpl[P#Raw, titan.TitanElement] =
  new PropertyImpl[P#Raw, titan.TitanElement] {

    def get(e: Element, p: P): Property = {
      e.getProperty[p.Raw](p.label)
    }

    def lookup(p: Property): Element = ???
  }

}
