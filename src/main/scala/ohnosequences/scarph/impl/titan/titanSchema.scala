package ohnosequences.scarph.impl.titan

object titanSchema {

  import ohnosequences.scarph.schemas._
  import ohnosequences.scarph.objects._
  import com.thinkaurelius.titan.{ core => titan }
  import titan.TitanGraph
  import titan.schema.TitanManagement
  import titan.Multiplicity

  import scala.reflect._
  // import scala.reflect.runtime.universe._

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
  // TODO return errors
  implicit def titanGraphSchemaOps(graph: TitanGraph): TitanGraphSchemaOps = TitanGraphSchemaOps(graph)

  final case class TitanGraphSchemaOps(val graph: TitanGraph) extends AnyVal {

    final def addPropertyKey(v: AnyValueType): titan.PropertyKey = {
      println(s"  Creating [${v.label}] property key (${v.valueTag})")

      graph.makePropertyKey(v.label)
        .dataType(v.valueTag.runtimeClass)
        .make()
    }

    final def addEdgeLabel(e: AnyEdge): titan.EdgeLabel = {
      println(s"  Creating [${e.label}] edge label")

      graph.makeEdgeLabel(e.label)
        .multiplicity(edgeTitanMultiplicity(e))
        .make()
    }

    final def addVertexLabel(v: AnyVertex): titan.VertexLabel = {
      println(s"  Creating [${v.label}] vertex label")

      graph.makeVertexLabel(v.label)
        .make()
    }

    // TODO: could return something more useful, for example pairs (scarph type, titan key)
    final def createSchema(schema: AnyGraphSchema): Unit = {

      println(s"  Creating schema types for ${schema.label}")

      val propertyKeys = schema.valueTypes map graph.addPropertyKey
      val edgeLabels   = schema.edges      map graph.addEdgeLabel
      val vertexLabels = schema.vertices   map graph.addVertexLabel

      // NOTE: not sure that it's needed
      // graph.commit
    }
  }
}
