package ohnosequences.scarph.impl.titan

object predicates {

  import com.tinkerpop.blueprints.Compare._
  import com.tinkerpop.blueprints

  import ohnosequences.cosas._, fns._
  import ohnosequences.cosas.ops.typeSets._

  import ohnosequences.{ scarph => s}
  import s.objects._


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

}
