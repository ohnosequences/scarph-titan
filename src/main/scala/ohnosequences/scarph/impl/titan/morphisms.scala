package ohnosequences.scarph.impl.titan

object morphisms {

  import ohnosequences.scarph._, impl._


  case class quantifyOutE[P <: AnyPredicate { type Element <: AnyEdge }]
    (val predicate: P) extends AnyGraphMorphism {

    type Predicate = P

    type Edge = Predicate#Element
    val  edge = predicate.element

    type     In = Edge#Source
    lazy val in = edge.source

    type     Out = Predicate
    lazy val out = predicate

    type     Dagger = source[Edge]
    lazy val dagger = source(edge): Dagger

    lazy val label: String = s"quantifyOutE(${predicate.label})"
  }

  case class quantifyInE[P <: AnyPredicate { type Element <: AnyEdge }]
    (val predicate: P) extends AnyGraphMorphism {

    type Predicate = P

    type Edge = Predicate#Element
    val  edge = predicate.element

    type     In = Edge#Target
    lazy val in = edge.target

    type     Out = Predicate
    lazy val out = predicate

    type     Dagger = target[Edge]
    lazy val dagger = target(edge): Dagger

    lazy val label: String = s"quantifyInE(${predicate.label})"
  }

}
