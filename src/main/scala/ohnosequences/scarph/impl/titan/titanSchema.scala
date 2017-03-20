package ohnosequences.scarph.impl.titan

object titanSchema {

  import ohnosequences.scarph._
  import com.thinkaurelius.titan.{ core => titan }
  import titan.TitanGraph
  import titan.schema.SchemaManager
  import titan.Multiplicity

  import scala.reflect._
  // import scala.reflect.runtime.universe._

  final def edgeTitanMultiplicity(a: AnyEdge): Multiplicity = a.sourceArity match {

    case oneOrNone(_) | exactlyOne(_)   => a.targetArity match {

      case oneOrNone(_)  | exactlyOne(_)  => Multiplicity.ONE2ONE
      case atLeastOne(_) | manyOrNone(_)  => Multiplicity.ONE2MANY
    }

    case atLeastOne(_) | manyOrNone(_)  => a.targetArity match {

      case oneOrNone(_)  | exactlyOne(_)  => Multiplicity.MANY2ONE
      case atLeastOne(_) | manyOrNone(_)  => Multiplicity.MULTI
    }
  }

  // TODO this should be improved
  /* These methods work in a context of a previously created schema manager transaction (see below) */
  implicit final class SchemaManagerSchemaOps(val manager: SchemaManager) extends AnyVal {

    final def addPropertyKey(v: AnyValueType): titan.PropertyKey = {
      println(s"  Creating [${v.label}] property key (${v.valueTag})")

      manager.makePropertyKey(v.label)
        .dataType(v.valueTag.runtimeClass)
        .make()
    }

    final def addEdgeLabel(e: AnyEdge): titan.EdgeLabel = {
      println(s"  Creating [${e.label}] edge label")

      manager.makeEdgeLabel(e.label)
        .multiplicity(edgeTitanMultiplicity(e))
        .make()
    }

    final def addVertexLabel(v: AnyVertex): titan.VertexLabel = {
      println(s"  Creating [${v.label}] vertex label")

      manager.makeVertexLabel(v.label)
        .make()
    }
  }

  /* This opens a new schema manager instance, create the schema and commits */
  implicit final class TitanGraphSchemaOps(val graph: TitanGraph) extends AnyVal {
    // TODO: could return something more useful, for example pairs (scarph type, titan key)
    final def createSchema(schema: AnyGraphSchema): Unit = {
      val manager = graph.openManagement

      println(s"  Creating schema types for ${schema.label}")

      val propertyKeys = schema.valueTypes map manager.addPropertyKey
      val edgeLabels   = schema.edges      map manager.addEdgeLabel
      val vertexLabels = schema.vertices   map manager.addVertexLabel

      manager.commit()
    }
  }
}
