
```scala
package ohnosequences.scarph.impl.titan

case object syntax {

  import types._
  import ohnosequences.cosas.types._
  import ohnosequences.scarph.objects._

  implicit def titanObjectValSyntax[F <: AnyGraphObject, VF <: AnyTitanType with F#Raw](vf: F := VF):
    TitanObjectValSyntax[F, VF] =
    TitanObjectValSyntax[F, VF](vf)

  case class TitanObjectValSyntax[F <: AnyGraphObject, VF <: AnyTitanType](vf: F := VF) extends AnyVal {

    def ⊗[S <: AnyGraphObject, VS <: AnyTitanType](vs: S := VS): (F ⊗ S) := Duplet[VF, VS] =
      (vf.tpe ⊗ vs.tpe) := Duplet(vf.value, vs.value)

    def ⊕[S <: AnyGraphObject, VS <: AnyTitanType](vs: S := VS): (F ⊕ S) := Duplet[VF, VS] =
      (vf.tpe ⊕ vs.tpe) := Duplet(vf.value, vs.value)
  }
}

```




[main/scala/ohnosequences/scarph/impl/titan/evals.scala]: evals.scala.md
[main/scala/ohnosequences/scarph/impl/titan/morphisms.scala]: morphisms.scala.md
[main/scala/ohnosequences/scarph/impl/titan/predicates.scala]: predicates.scala.md
[main/scala/ohnosequences/scarph/impl/titan/rewrites.scala]: rewrites.scala.md
[main/scala/ohnosequences/scarph/impl/titan/syntax.scala]: syntax.scala.md
[main/scala/ohnosequences/scarph/impl/titan/titanSchema.scala]: titanSchema.scala.md
[main/scala/ohnosequences/scarph/impl/titan/types.scala]: types.scala.md
[test/scala/ohnosequences/scarph/titan/schemaTests.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/schemaTests.scala.md
[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala.md