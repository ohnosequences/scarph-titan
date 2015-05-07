package ohnosequences.scarph.impl.titan.test

import ohnosequences.scarph._, objects._
import com.thinkaurelius.titan.{ core => titan }
//import scala.reflect.runtime.universe._*/


object schemaTests {

  // import ohnosequences.scarph.test._

  case object uh extends ValueOfType[String]("uh")
  case object ah extends Vertex("hola!") {
    case object name extends Property(ah -> uh)("argh")
    case object aaaa extends Property(ah -> uh)("aaaa")
    case object bbbb extends Property(ah -> uh)("bbbb")
  }
}

class TitanSuite extends org.scalatest.FunSuite with org.scalatest.BeforeAndAfterAll {

  import ohnosequences.scarph._, evals._, morphisms._
  import syntax.objects._, syntax.morphisms._
  import ohnosequences.scarph.impl.titan.evals._

  object titanTwitterEvals extends DefaultTitanEvals { val graph = g }
  import titanTwitterEvals._

  test("eval basic queries over sample twitter graph") {

    import ohnosequences.scarph.test._, twitter._
    import ohnosequences.scarph.impl.titan.implementations._
    import ohnosequences.scarph.impl.titan.types._

    val query = lookup(user.name)
      .outV(posted)
      //.quantify(tweet ? (tweet.url =/= "foo"))
      //.coerce
      //.get(tweet.text)
      .inE(posted)
      .quantify(posted ? (posted.time =/= ""))
      .coerce
      .get(posted.time)

    implicit def toCont[T](ts: Seq[T]): Container[T] = ts

    println("\n----------------")
    println("rewritten query:")
    println(evaluate(rewrite(query)).evalPlan)

    lazy val z = evaluate(query) on (
      name := Seq("@laughedelic", "@eparejatobes", "@evdokim")
    )

    println("\n----------------")
    println("predicates:")
    //z.value.foreach{ p => println(p.asInstanceOf[com.thinkaurelius.titan.graphdb.query.vertex.VertexCentricQueryBuilder].describeForEdges.toString) }*/
    //z.value.foreach{ p => println(p.asInstanceOf[com.thinkaurelius.titan.graphdb.query.vertex.VertexCentricQueryBuilder].describeForProperties.toString) }*/

    println("\n----------------")
    println("results:")
    z.value.foreach(println)

    println("\n----------------")
  }


  import java.io.File
  import com.thinkaurelius.titan.core._

  val graphLocation = new File("/tmp/titanTest")
  var g: TitanGraph = null

  override final def beforeAll() {

    def cleanDir(f: File) {
      if (f.isDirectory) f.listFiles.foreach(cleanDir(_))
      else { println(f.toString); f.delete }
    }
    cleanDir(graphLocation)
    g = TitanFactory.open("berkeleyje:" + graphLocation.getAbsolutePath)

    println("Created Titan graph")


    import ohnosequences.scarph.impl.titan.titanSchema._
    import ohnosequences.scarph.test._

    g.createSchema(twitter)

    import com.tinkerpop.blueprints.util.io.graphson._
    GraphSONReader.inputGraph(g, this.getClass.getResource("/twitter_graph.json").getPath)

    println("loaded sample Twitter data")
  }

  override final def afterAll() {
    if(g != null) {
      // NOTE: uncommend if you want to add data to the GraphSON:
      // import com.tinkerpop.blueprints.util.io.graphson._
      // GraphSONWriter.outputGraph(g, "graph_compact.json", GraphSONMode.COMPACT)

      g.shutdown
      println("Shutdown Titan graph")
    }
  }
}
