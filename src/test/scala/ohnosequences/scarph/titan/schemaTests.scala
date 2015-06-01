package ohnosequences.scarph.impl.titan.test

import ohnosequences.scarph._, objects._
import com.thinkaurelius.titan.{ core => titan }
//import scala.reflect.runtime.universe._*/


class TitanSuite extends org.scalatest.FunSuite with org.scalatest.BeforeAndAfterAll {

  import ohnosequences.scarph._, evals._, morphisms._, rewrites._
  import syntax.objects._, syntax.morphisms._
  import ohnosequences.scarph.impl.titan.evals._

  // object titanTwitterEvals extends DefaultTitanEvals { val graph = g }
  // import titanTwitterEvals._
  val impl = ohnosequences.scarph.impl.titan.titan.all(null: titan.TitanGraph); import impl._
  // import ohnosequences.scarph.impl.titan.evals.categoryStructure._
  // val tstr = ohnosequences.scarph.impl.titan.evals.tensorStructure(null: titan.TitanGraph); import tstr._
  // import ohnosequences.scarph.impl.titan.evals.graphStructure._
  // import ohnosequences.scarph.impl.titan.evals.biproductStructure._
  // val vstr = ohnosequences.scarph.impl.titan.evals.vertexPropertyStructure[Integer](null: titan.TitanGraph); import vstr._

  test("eval basic queries over sample twitter graph") {

    import ohnosequences.scarph.test._, twitter._
    import ohnosequences.scarph.impl.titan.implementations._
    import ohnosequences.scarph.impl.titan.types._

    //val query = lookup(user.name)*/
      //.outV(posted)*/
      //.quantify(tweet ? (tweet.url =/= "foo"))
      //.coerce
      //.get(tweet.text)
      //.inV(posted)*/
      //.filter(user ? (user.name =/= "@evdokim") and (user.age > 20))*/
      //.duplicate*/
      //.andThen(get(user.name) ⊗ get(user.name))*/
      //.matchUp
      //.get(user.name)
      //.inE(posted)
      //.quantify(posted ? (posted.time =/= ""))
      //.coerce
      //.get(posted.time)

    val query1 =
      lookup(user.name) >=>
      inV(follows) >=>
      quantify(user ? (user.age > 10)) >=>
      coerce(user ? (user.age > 10)) >=>
      fork(user) >=>
      (outV(follows) ⊕ inV(follows)) >=>
      (outV(posted) ⊕ outV(posted)) >=>
      merge(tweet) >=>
      get(tweet.text)

    val query2 =
      lookup(user.name)
      .inV(follows)
      .filter(user ? (user.age > 10))
      .fork
      .andThen(outV(follows) ⊕ inV(follows))
      .andThen(outV(posted) ⊕ outV(posted))
      .merge
      .get(tweet.text)

    assert(query1 == query2)

    println("\n----------------")
    println("rewritten query:")
    // println(rewrite(query2).label)

    println(evalOn[Container[String]](query2).evalPlan)

    //lazy val z = evaluate(query) on (
    //  name := Seq("@laughedelic", "@eparejatobes", "@evdokim")
    //)

    println("\n----------------")
    println("results:")
    //z.value.foreach(println)*/

    println("\n----------------")
  }


  import java.io.File
  import com.thinkaurelius.titan.core._

  val graphLocation = new File("/tmp/titanTest")
  var g: titan.TitanGraph = null

  /*override final def beforeAll() {

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
  }*/
}
