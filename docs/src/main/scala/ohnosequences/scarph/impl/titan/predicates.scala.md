
```scala
package ohnosequences.scarph.impl.titan

object predicates {

  import com.thinkaurelius.titan.{ core => titan }
  import com.thinkaurelius.titan.core.attribute.Cmp._
  import com.thinkaurelius.titan.graphdb.query.condition.PredicateCondition
  import ohnosequences.scarph._, impl._


  // TODO: AnyCondition can be converted to PredicateCondition, then AnyPredicate can be converted to an And combination of its condition
  // See http://titan.thinkaurelius.com/javadoc/current/com/thinkaurelius/titan/graphdb/query/condition/Condition.html

  // evaluates conjunction of all predicate's conditions on a given titan element
  def evalPredicate[P <: AnyPredicate, TE <: titan.TitanElement](predicate: P, elem: TE): Boolean = {
    predicate.conditions.foldLeft(true){ (acc, c) =>
      c match {
        case Equal(p, value)          => acc && PredicateCondition.of(p.label, EQUAL,              value).evaluate(elem)
        case NotEqual(p, value)       => acc && PredicateCondition.of(p.label, NOT_EQUAL,          value).evaluate(elem)
        case Less(p, value)           => acc && PredicateCondition.of(p.label, LESS_THAN,          value).evaluate(elem)
        case LessOrEqual(p, value)    => acc && PredicateCondition.of(p.label, LESS_THAN_EQUAL,    value).evaluate(elem)
        case Greater(p, value)        => acc && PredicateCondition.of(p.label, GREATER_THAN,       value).evaluate(elem)
        case GreaterOrEqual(p, value) => acc && PredicateCondition.of(p.label, GREATER_THAN_EQUAL, value).evaluate(elem)
        case _ => acc
      }
    }
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
[main/scala/ohnosequences/scarph/impl/titan/writes.scala]: writes.scala.md
[test/scala/ohnosequences/scarph/titan/SchemaCreation.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/SchemaCreation.scala.md
[test/scala/ohnosequences/scarph/titan/schemaTests.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/schemaTests.scala.md
[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala.md