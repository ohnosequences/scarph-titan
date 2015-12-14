// package ohnosequences.scarph.impl.titan
//
// case object rewrites {
//
//   import morphisms._
//
//   import ohnosequences.{ scarph => s }
//   import s.objects._, s.morphisms._, s.rewrites._
//
//   case object edgeQuantification extends AnyRewriteStrategy {
//
//     implicit def quantified_outE[
//       E <: AnyEdge,
//       P <: AnyPredicate { type Element = E }
//     ]: (outE[E] >=> quantify[P]) rewriteTo
//        (quantifyOutE[P])
//     = rewriteTo({ o_q =>
//
//         quantifyOutE(o_q.second.predicate)
//       })
//
//
//     implicit def m_quantified_outE[
//       M <: AnyGraphMorphism { type Out = E#SourceVertex },
//       E <: AnyEdge,
//       P <: AnyPredicate { type Element = E }
//     ]: (M >=> outE[E] >=> quantify[P]) rewriteTo
//        (M >=> quantifyOutE[P])
//     = rewriteTo({ mo_q =>
//
//         val m = mo_q.first.first
//         val q = mo_q.second
//
//         m >=> quantifyOutE(q.predicate)
//       })
//
//     implicit def quantified_inE[
//       E <: AnyEdge,
//       P <: AnyPredicate { type Element = E }
//     ]: (inE[E] >=> quantify[P]) rewriteTo
//        (quantifyInE[P])
//     = rewriteTo({ i_q =>
//
//         quantifyInE(i_q.second.predicate)
//       })
//
//
//     implicit def m_quantified_inE[
//       M <: AnyGraphMorphism { type Out = E#TargetVertex },
//       E <: AnyEdge,
//       P <: AnyPredicate { type Element = E }
//     ]: (M >=> inE[E] >=> quantify[P]) rewriteTo
//        (M >=> quantifyInE[P])
//     = rewriteTo({ mi_q =>
//
//         val m = mi_q.first.first
//         val q = mi_q.second
//
//         m >=> quantifyInE(q.predicate)
//       })
//
//   }
//
// }
