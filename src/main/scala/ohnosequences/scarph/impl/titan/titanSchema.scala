package ohnosequences.scarph.impl.titan

object titanSchema {

  import ohnosequences.scarph._
  import com.thinkaurelius.titan.{ core => titan }
  import titan.TitanGraph
  import titan.schema.{ SchemaManager, TitanManagement, TitanGraphIndex }
  import titan.{ Multiplicity, Cardinality }

  import scala.reflect._
  import scala.util._
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
  implicit final class SchemaManagerSchemaOps(val schemaManager: SchemaManager) extends AnyVal {

    final def addPropertyKey(v: AnyValueType): titan.PropertyKey = {
      println(s"  Creating [${v.label}] property key (${v.valueTag})")

      schemaManager.makePropertyKey(v.label)
        .cardinality( Cardinality.SINGLE )
        .dataType(v.valueTag.runtimeClass)
        .make()
    }

    final def addEdgeLabel(e: AnyEdge): titan.EdgeLabel = {
      println(s"  Creating [${e.label}] edge label")

      schemaManager.makeEdgeLabel(e.label)
        .directed()
        .multiplicity(edgeTitanMultiplicity(e))
        .make()
    }

    final def addVertexLabel(v: AnyVertex): titan.VertexLabel = {
      println(s"  Creating [${v.label}] vertex label")

      schemaManager.makeVertexLabel(v.label)
        .make()
    }

    // TODO: could return something more useful, for example pairs (scarph type, titan key)
    final def createSchema(schema: AnyGraphSchema): Unit = {
      println(s"  Creating schema types for ${schema.label}")

      val propertyKeys = schema.valueTypes map schemaManager.addPropertyKey
      val edgeLabels   = schema.edges      map schemaManager.addEdgeLabel
      val vertexLabels = schema.vertices   map schemaManager.addVertexLabel
    }
  }

  /* This is similar to SchemaManagerOps, but can create indexes */
  implicit final class TitanManagementOps(val manager: TitanManagement) extends AnyVal {

    def createIndex(property: AnyProperty): TitanGraphIndex = {
      val ownerClass = property.source.elementType match {
        case VertexElement => classOf[titan.TitanVertex]
        case EdgeElement   => classOf[titan.TitanEdge]
      }

      val indexBuilder = manager
          .buildIndex(s"${property.label}.index", ownerClass)
          .addKey(manager.getPropertyKey( property.target.label ))

      val withUniqueness = property.targetArity match {
        case oneOrNone(_) | exactlyOne(_) => indexBuilder.unique()
        case _                            => indexBuilder
      }

      withUniqueness.buildCompositeIndex()
    }

  }

  /* This opens a new schema manager instance, create the schema and commits */
  implicit final class TitanGraphOps(val graph: TitanGraph) extends AnyVal {


    def withManager[T](fn: TitanManagement => T): Try[T] = {
      val manager = graph.openManagement

      val result = Try {
        val t = fn(manager)
        manager.commit()
        t
      }

      result match {
        case Failure(_) => manager.rollback(); result
        case Success(_) => result
      }
    }

    def createSchema(schema: AnyGraphSchema): Try[Unit] = withManager { _.createSchema(schema) }

    def createIndex(property: AnyProperty): Try[TitanGraphIndex] = withManager { _.createIndex(property) }
  }
}
