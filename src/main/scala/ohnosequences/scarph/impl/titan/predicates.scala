package ohnosequences.scarph.impl.titan

object predicates {

  import com.tinkerpop.blueprints.Compare._
  import com.tinkerpop.blueprints.{ Query => BQuery }
  // import com.thinkaurelius.titan.core.{ TitanVertexQuery => BQuery }

  import ohnosequences.cosas._, fns._
  import ohnosequences.cosas.ops.typeSets._

  import ohnosequences.{ scarph => s}
  import s.graphTypes._, s.conditions._, s.predicates._


  trait ToBlueprintsQuery[P <: AnyPredicate] extends Fn2[P, BQuery] with Out[BQuery]

  object ToBlueprintsQuery {

    implicit def convert[P <: AnyPredicate]
      (implicit toList: ToList[P#Conditions] { type O = AnyCondition }):
        ToBlueprintsQuery[P] =
    new ToBlueprintsQuery[P] {
      def apply(p: In1, q: In2): Out = {

        toList(p.conditions).foldLeft(q){ (acc, c) =>
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
  }

}
