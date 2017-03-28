
```scala
// package ohnosequences.scarph.impl.titan
//
// case object rewrites {
//
//   import morphisms._
//
//   import ohnosequences.scarph._, impl._ //, rewrites._
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
//       M <: AnyGraphMorphism { type Out = E#Source },
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
//       M <: AnyGraphMorphism { type Out = E#Target },
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

```




[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala.md
[test/scala/ohnosequences/scarph/titan/schemaTests.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/schemaTests.scala.md
[test/scala/ohnosequences/scarph/titan/SchemaCreation.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/SchemaCreation.scala.md
[main/scala/ohnosequences/scarph/impl/titan/predicates.scala]: predicates.scala.md
[main/scala/ohnosequences/scarph/impl/titan/types.scala]: types.scala.md
[main/scala/ohnosequences/scarph/impl/titan/morphisms.scala]: morphisms.scala.md
[main/scala/ohnosequences/scarph/impl/titan/evals.scala]: evals.scala.md
[main/scala/ohnosequences/scarph/impl/titan/syntax.scala]: syntax.scala.md
[main/scala/ohnosequences/scarph/impl/titan/writes.scala]: writes.scala.md
[main/scala/ohnosequences/scarph/impl/titan/rewrites.scala]: rewrites.scala.md
[main/scala/ohnosequences/scarph/impl/titan/titanSchema.scala]: titanSchema.scala.md