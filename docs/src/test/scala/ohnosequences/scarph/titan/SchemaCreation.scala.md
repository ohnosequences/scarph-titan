
```scala
package ohnosequences.scarph.impl.titan.test

import com.thinkaurelius.titan.core
import ohnosequences.scarph.test.twitter
import ohnosequences.scarph.impl.titan.schema._
import java.io.File

class SchemaCreation extends org.scalatest.FunSuite {

  test("create all types") {

    val tGraph =
      configuration.open()

    val vertices =
      twitter.vertices.toList

    val edges =
      twitter.edges.toList

    val properties =
      twitter.properties.toList

    val createdTypesT =
      tGraph.withManager { mgmt =>

        val vlbls =
          vertices map mgmt.createOrGetVertexLabel

        val elbls =
          edges map mgmt.createOrGetEdgeLabel

        val pks =
          properties map mgmt.createOrGetPropertyKey

        (vlbls, elbls, pks)
      }

    val zzz =
      tGraph.close()

    val tGraphAfter =
      configuration.open()

    val checks =
      tGraphAfter.withManager { mgmt =>

        assert {
          noNone { vertices map mgmt.vertexLabel    } &&
          noNone { edges map mgmt.edgeLabel         } &&
          noNone { properties map mgmt.propertyKey  } &&
          noNone { properties map mgmt.indexFor     }
        }
      }

    tGraphAfter.close()
    cleanDB
  }

  def noNone[X](xs: Seq[Option[X]]): Boolean =
    xs contains { opt: Option[X] => !opt.isEmpty }

  def cleanDB: Unit = {

    val file = new File("db/")
    if (file.isDirectory) {
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(_.delete)
    }

    file.delete
  }

  val configuration: core.TitanFactory.Builder =
  core.TitanFactory.build()
    .set(                      "schema.default",  "none"          )
    .set(                     "storage.backend",  "berkeleyje"    )
    .set(                   "storage.directory",  "db"            )
    .set(                "storage.transactions",  true            )
    .set( "storage.berkeleyje.cache-percentage",  20              )
    .set(  "storage.berkeleyje.isolation-level",  "SERIALIZABLE"  )
}

```




[main/scala/ohnosequences/scarph/impl/titan/evals.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/evals.scala.md
[main/scala/ohnosequences/scarph/impl/titan/morphisms.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/morphisms.scala.md
[main/scala/ohnosequences/scarph/impl/titan/predicates.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/predicates.scala.md
[main/scala/ohnosequences/scarph/impl/titan/rewrites.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/rewrites.scala.md
[main/scala/ohnosequences/scarph/impl/titan/syntax.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/syntax.scala.md
[main/scala/ohnosequences/scarph/impl/titan/titanSchema.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/titanSchema.scala.md
[main/scala/ohnosequences/scarph/impl/titan/types.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/types.scala.md
[main/scala/ohnosequences/scarph/impl/titan/writes.scala]: ../../../../../main/scala/ohnosequences/scarph/impl/titan/writes.scala.md
[test/scala/ohnosequences/scarph/titan/SchemaCreation.scala]: SchemaCreation.scala.md
[test/scala/ohnosequences/scarph/titan/schemaTests.scala]: schemaTests.scala.md
[test/scala/ohnosequences/scarph/titan/TwitterTitanTest.scala]: TwitterTitanTest.scala.md