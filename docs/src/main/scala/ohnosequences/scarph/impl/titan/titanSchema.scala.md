
```scala
package ohnosequences.scarph.impl.titan

import ohnosequences.scarph._
import com.thinkaurelius.titan.{ core => titan }
import titan.{ TitanTransaction, TitanGraphTransaction, TitanGraph }
import titan.{ Multiplicity, Cardinality }
import titan.schema.{ SchemaManager, TitanManagement, TitanGraphIndex }

import scala.reflect._
import scala.util._

case object schema {

  // TODO this should be improved

```

These methods work in a context of a previously created schema manager transaction (see below)

```scala
  implicit final class TitanManagementOps(val schemaManager: TitanManagement) extends AnyVal {

    final def vertexLabel(v: AnyVertex): Option[titan.VertexLabel] =
      Option(schemaManager getVertexLabel v.label)

    final def createOrGetVertexLabel(v: AnyVertex): titan.VertexLabel =
      vertexLabel(v) getOrElse {
        schemaManager
          .makeVertexLabel(v.label)
          .make()
      }

    final def edgeLabel(e: AnyEdge): Option[titan.EdgeLabel] =
      Option(schemaManager getEdgeLabel e.label)

    final def createOrGetEdgeLabel(e: AnyEdge): titan.EdgeLabel =
      edgeLabel(e) getOrElse {
        schemaManager
          .makeEdgeLabel(e.label)
          .directed()
          .multiplicity(edgeTitanMultiplicity(e))
          .make()
      }

    final def propertyKey(p: AnyProperty): Option[titan.PropertyKey] =
      Option(schemaManager getPropertyKey p.label)

    final def createOrGetPropertyKey(p: AnyProperty): titan.PropertyKey =
      propertyKey(p) getOrElse {

        val clzzFromTag: Class[_] =
          p.targetArity.graphObject.valueTag.runtimeClass

        val clzz: Class[_] =
          primitivesToBoxed.get(clzzFromTag) getOrElse clzzFromTag

        val propertyKey: titan.PropertyKey =
          Option(schemaManager getPropertyKey p.label) getOrElse
            schemaManager
            .makePropertyKey(p.label)
            .cardinality( Cardinality.SINGLE )
            .dataType(clzz)
            .make()

        // why here? because https://github.com/thinkaurelius/titan/issues/793#issuecomment-60698050
        val index =
          createOrGetIndexFor(p)

        propertyKey
      }

    def indexNameFor(property: AnyProperty): String =
      s"${property.label}_index"

    final def indexFor(p: AnyProperty): Option[TitanGraphIndex] =
      Option(schemaManager getGraphIndex indexNameFor(p))

    final def createOrGetIndexFor(property: AnyProperty): TitanGraphIndex =
      indexFor(property) getOrElse {

        val elementClass: Class[_ <: org.apache.tinkerpop.gremlin.structure.Element] =
          property.source.elementType match {
            case VertexElement => classOf[titan.TitanVertex]
            case EdgeElement   => classOf[titan.TitanEdge]
          }

        val propertyKey: titan.PropertyKey =
          schemaManager getPropertyKey property.label

        val genericConf =
          schemaManager
            .buildIndex(indexNameFor(property), elementClass)
            .addKey(propertyKey)
            // .indexOnly(
            //   property.source.elementType match {
            //     case VertexElement => schemaManager getVertexLabel property.source.label
            //     case EdgeElement   => schemaManager getEdgeLabel property.source.label
            //   }
            // )

        val indexBuilder =
          property.sourceArity match {
            case oneOrNone(_) | exactlyOne(_) => genericConf.unique()
            case _                            => genericConf
          }

        indexBuilder.buildCompositeIndex()
      }

    // TODO: could return something more useful, for example pairs (scarph type, titan key)
    final def createOrGetSchema(schema: AnyGraphSchema): Unit = {

      val vertexLabels =
        schema.vertices.toSeq map schemaManager.createOrGetVertexLabel

      val edgeLabels   =
        schema.edges.toSeq map schemaManager.createOrGetEdgeLabel

      val props =
        schema.properties.toSeq map schemaManager.createOrGetPropertyKey
    }
  }
```

This opens a new schema manager instance, create the schema and commits

```scala
  implicit final class TitanGraphOps(val graph: TitanGraph) extends AnyVal {
```

This is useful for wrapping writing operations

```scala
    def withTransaction[X](fn: TitanGraphTransaction => X): Try[X] = {

      val tx = graph.newTransaction()

      val result = Try {
        val x = fn(tx)
        tx.commit()
        x
      }

      result match {
        case Failure(err) => println(s"${err}"); tx.rollback(); result
        case Success(_) => result
      }
    }
```

Same as withTransaction, but uses TitanManagement (they don't have a common super-interface)

```scala
    def withManager[T](fn: TitanManagement => T): Try[T] = {

      val tx = graph.openManagement()

      val result = Try {
        val x = fn(tx)
        tx.commit()
        x
      }

      result match {
        case Failure(_) => tx.rollback(); result
        case Success(_) => result
      }
    }

    def createSchema(schema: AnyGraphSchema): Try[Unit] =
      withManager { _ createOrGetSchema schema }
  }

  private val primitivesToBoxed: Map[Class[_], Class[_ <: AnyRef]] =
    Map[Class[_], Class[_ <: AnyRef]](
      classOf[Int]      -> classOf[java.lang.Integer],
      classOf[Long]     -> classOf[java.lang.Long],
      classOf[Boolean]  -> classOf[java.lang.Boolean],
      classOf[Float]    -> classOf[java.lang.Float],
      classOf[Double]   -> classOf[java.lang.Double],
      classOf[Char]     -> classOf[java.lang.Character],
      classOf[Byte]     -> classOf[java.lang.Byte],
      classOf[Short]    -> classOf[java.lang.Short]
    )

  final def edgeTitanMultiplicity(a: AnyEdge): Multiplicity =
    a.sourceArity match {

      case oneOrNone(_) | exactlyOne(_)   => a.targetArity match {

        case oneOrNone(_)  | exactlyOne(_)  => Multiplicity.ONE2ONE
        case atLeastOne(_) | manyOrNone(_)  => Multiplicity.ONE2MANY
      }

      case atLeastOne(_) | manyOrNone(_)  => a.targetArity match {

        case oneOrNone(_)  | exactlyOne(_)  => Multiplicity.MANY2ONE
        case atLeastOne(_) | manyOrNone(_)  => Multiplicity.MULTI
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