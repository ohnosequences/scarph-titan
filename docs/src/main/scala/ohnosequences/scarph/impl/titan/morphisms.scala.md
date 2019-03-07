
```scala
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

```




[main/scala/ohnosequences/scarph/impl/titan/predicates.scala]: predicates.scala.md
[main/scala/ohnosequences/scarph/impl/titan/types.scala]: types.scala.md
[main/scala/ohnosequences/scarph/impl/titan/rewrites.scala]: rewrites.scala.md
[main/scala/ohnosequences/scarph/impl/titan/syntax.scala]: syntax.scala.md
[main/scala/ohnosequences/scarph/impl/titan/evals.scala]: evals.scala.md
[main/scala/ohnosequences/scarph/impl/titan/writes.scala]: writes.scala.md
[main/scala/ohnosequences/scarph/impl/titan/morphisms.scala]: morphisms.scala.md
[main/scala/ohnosequences/scarph/impl/titan/titanSchema.scala]: titanSchema.scala.md
[test/scala/ohnosequences/scarph/titan/schemaTests.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/schemaTests.scala.md
[test/scala/ohnosequences/scarph/titan/SchemaCreation.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/SchemaCreation.scala.md
[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala.md