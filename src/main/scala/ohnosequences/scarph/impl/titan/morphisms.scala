// package ohnosequences.scarph.impl.titan
//
// object morphisms {
//
//   import ohnosequences.{ scarph => s}
//   import s.objects._, s.morphisms._
//
//
//   case class quantifyOutE[P <: AnyPredicate { type Element <: AnyEdge }]
//     (val predicate: P) extends AnyGraphMorphism {
//
//     type Predicate = P
//
//     type Edge = Predicate#Element
//     val  edge = predicate.element
//
//     type     In = Edge#SourceVertex
//     lazy val in = edge.sourceVertex
//
//     type     Out = Predicate
//     lazy val out = predicate
//
//     type     Dagger = source[Edge]
//     lazy val dagger = source(edge): Dagger
//
//     lazy val label: String = s"quantifyOutE(${predicate.label})"
//   }
//
//   case class quantifyInE[P <: AnyPredicate { type Element <: AnyEdge }]
//     (val predicate: P) extends AnyGraphMorphism {
//
//     type Predicate = P
//
//     type Edge = Predicate#Element
//     val  edge = predicate.element
//
//     type     In = Edge#TargetVertex
//     lazy val in = edge.targetVertex
//
//     type     Out = Predicate
//     lazy val out = predicate
//
//     type     Dagger = target[Edge]
//     lazy val dagger = target(edge): Dagger
//
//     lazy val label: String = s"quantifyInE(${predicate.label})"
//   }
//
// }
