package ohnosequences.scarph.impl.titan

case object syntax {

  import types._
  import ohnosequences.cosas.types._
  import ohnosequences.scarph._, impl._


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
