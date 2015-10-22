package ohnosequences.scarph.impl.titan

case object syntax {

  import types._
  import ohnosequences.cosas.types._
  import ohnosequences.scarph.objects._

  implicit def titanObjectValSyntax[F <: AnyGraphObject, VF <: AnyTitanType with F#Raw](vf: F := VF):
    TitanObjectValSyntax[F, VF] =
    TitanObjectValSyntax[F, VF](vf.value)

  case class TitanObjectValSyntax[F <: AnyGraphObject, VF <: AnyTitanType](vf: VF) extends AnyVal {

    def ⊗[S <: AnyGraphObject, VS <: AnyTitanType with S#Raw](vs: S := VS): (F ⊗ S) := Duplet[VF, VS] =
      new Denotes( Duplet(vf, vs.value) )

    def ⊕[S <: AnyGraphObject, VS <: AnyTitanType with S#Raw](vs: S := VS): (F ⊕ S) := Duplet[VF, VS] =
      new Denotes( Duplet(vf, vs.value) )
  }
}
