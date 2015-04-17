package ohnosequences.scarph.impl.titan

object titanSchema {

  import ohnosequences.scarph.schemas._
  import ohnosequences.scarph.graphTypes._
  import com.thinkaurelius.titan.{ core => titan }
  import titan.TitanGraph
  import titan.schema.TitanManagement
  import titan.Multiplicity

  import scala.reflect._
  import scala.reflect.runtime.universe._

  final def edgeTitanMultiplicity(a: AnyEdge): Multiplicity = a.sourceArity match {

    case OneOrNone(_) | ExactlyOne(_)   => a.targetArity match {

      case OneOrNone(_)  | ExactlyOne(_)  => Multiplicity.ONE2ONE
      case AtLeastOne(_) | ManyOrNone(_)  => Multiplicity.ONE2MANY
    }

    case AtLeastOne(_) | ManyOrNone(_)  => a.targetArity match {

      case OneOrNone(_)  | ExactlyOne(_)  => Multiplicity.MANY2ONE
      case AtLeastOne(_) | ManyOrNone(_)  => Multiplicity.MULTI
    }
  }

  // TODO this should be improved
  implicit def titanGraphSchemaOps(g: TitanGraph): TitanGraphSchemaOps = TitanGraphSchemaOps(g)
  final case class TitanGraphSchemaOps(val g: TitanGraph) extends AnyVal {

    // TODO return errors
    final def titanPropertyTypeFor(v: AnyValueType): Unit = {

      println(s"Creating property type for ${v.label}, with value type ${v.rawTag}")
      val mgmt = g.getManagementSystem

      mgmt.makePropertyKey(v.label)
        .dataType(v.rawTag.runtimeClass)
        .make()

      mgmt.commit
    }

    final def titanEdgeTypeFor(e: AnyEdge): Unit = {

      println(s"Creating edge type for ${e}")
      val mgmt = g.getManagementSystem

      mgmt.makeEdgeLabel(e.label)
        .multiplicity(edgeTitanMultiplicity(e))
        .make()

      mgmt.commit
    }

    final def titanVertexTypeFor(v: AnyVertex): Unit = {

      println(s"Creating vertex type for ${v}")
      val mgmt = g.getManagementSystem

      mgmt.makeVertexLabel(v.label)
        .make()

      mgmt.commit
    }
  }

  implicit final def scarphSchemaTitanOps(schema: AnyGraphSchema): ScarphSchemaTitanOps = ScarphSchemaTitanOps(schema)
  case class ScarphSchemaTitanOps(val schema: AnyGraphSchema) extends AnyVal {

    // TODO errors
    final def createTypesIn(g: TitanGraph)(implicit ops: TitanGraph => TitanGraphSchemaOps): Unit = {

      schema.vertices   map g.titanVertexTypeFor
      schema.edges      map g.titanEdgeTypeFor
      schema.valueTypes map g.titanPropertyTypeFor
    }
  }
}