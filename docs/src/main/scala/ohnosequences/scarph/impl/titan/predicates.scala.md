
```scala
package ohnosequences.scarph.impl.titan

object predicates {

  import com.thinkaurelius.titan.{ core => titan }
  import com.tinkerpop.blueprints.Compare._
  import com.tinkerpop.blueprints

  import ohnosequences.{ scarph => s}
  import s.objects._


  // extends query with conditions from the predicate
  def addConditions[P <: AnyPredicate](predicate: P, query: blueprints.Query): blueprints.Query = {
    predicate.conditions.foldLeft(query){ (acc, c) =>
      c match {
        case Equal(prop, value)          => acc.has(prop.label, EQUAL, value)
        case NotEqual(prop, value)       => acc.has(prop.label, NOT_EQUAL, value)
        case Less(prop, value)           => acc.has(prop.label, LESS_THAN, value)
        case LessOrEqual(prop, value)    => acc.has(prop.label, LESS_THAN_EQUAL, value)
        case Greater(prop, value)        => acc.has(prop.label, GREATER_THAN, value)
        case GreaterOrEqual(prop, value) => acc.has(prop.label, GREATER_THAN_EQUAL, value)
        // TODO: for interval we need that condition.Property.Raw <: Comparable[_]
        //case Interval(prop, start, end)  => acc.interval(prop.label, start, end)*/
        // skipping unknown conditions:
        case _ => acc
      }
    }
  }

  // evaluates conjunction of all predicate's conditions on a given titan element
  def evalPredicate[P <: AnyPredicate, TE <: titan.TitanElement](predicate: P, elem: TE): Boolean = {
    predicate.conditions.foldLeft(true){ (acc, c) =>
      c match {
        case Equal(p, value)          => acc &&              EQUAL.evaluate(elem.getProperty[p.target.Raw](p.label), value)
        case NotEqual(p, value)       => acc &&          NOT_EQUAL.evaluate(elem.getProperty[p.target.Raw](p.label), value)
        case Less(p, value)           => acc &&          LESS_THAN.evaluate(elem.getProperty[p.target.Raw](p.label), value)
        case LessOrEqual(p, value)    => acc &&    LESS_THAN_EQUAL.evaluate(elem.getProperty[p.target.Raw](p.label), value)
        case Greater(p, value)        => acc &&       GREATER_THAN.evaluate(elem.getProperty[p.target.Raw](p.label), value)
        case GreaterOrEqual(p, value) => acc && GREATER_THAN_EQUAL.evaluate(elem.getProperty[p.target.Raw](p.label), value)
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
[test/scala/ohnosequences/scarph/titan/schemaTests.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/schemaTests.scala.md
[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: ../../../../../../test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala.md