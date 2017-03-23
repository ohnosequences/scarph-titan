package ohnosequences.scarph.impl.titan.test

import com.thinkaurelius.titan.core
import core.TitanGraphTransaction
import scala.collection.JavaConverters.{ asJavaIterableConverter, iterableAsScalaIterableConverter }

import ohnosequences.scarph._
import ohnosequences.scarph.impl._
import ohnosequences.scarph.syntax._

import ohnosequences.scarph.test._
import ohnosequences.scarph.test.twitter._

import ohnosequences.scarph.impl.{ titan => t }
import t.types._, t.evals._, t.syntax._, t.writes._, t.titanSchema._
import java.io.File

class TitanSuite extends org.scalatest.FunSuite with org.scalatest.BeforeAndAfterAll {

  val twitterGraph = core.TitanFactory.open("inmemory")

  override final def beforeAll() = {

    /*
    def cleanDir(f: File) {
      if (f.isDirectory) f.listFiles.foreach(cleanDir(_))
      else { println(f.toString); f.delete }
    }
    cleanDir(graphLocation)
    */

    // twitterGraph = TitanFactory.open("berkeleyje:" + graphLocation.getAbsolutePath)

    println("Created Titan graph")

    twitterGraph.createSchema(twitter)

    twitterGraph.createIndex(twitter.user.name)
    twitterGraph.createIndex(twitter.tweet.url)

    twitterGraph.withTransaction { tx =>

      val w = titanScarph(tx); import w._
      val tw = unit := (tx); import tw._

      val bob = tw.add(user)
        .set(user.name, "Bob")
        .set(user.age, 92)

      val testTweet = tw.add(tweet)
        .set(tweet.text, "test")
        .set(tweet.url, "http://twitter.com/bob/1234")

      posted.add(bob, testTweet)
        .set(posted.time, "5 o'clock")

      // TODO: add some actual data for testing
    }

    // FIXME: old code for loading test graph doesn't work, because of incompatible Titan, new code doesn't work because of incompatible GraphSON format
    // import com.tinkerpop.blueprints.util.io.graphson._
    // GraphSONReader.inputGraph(twitterGraph, this.getClass.getResource("/twitter_graph.json").getPath)
    // import org.apache.tinkerpop.gremlin.structure.io.graphson._
    // GraphSONReader.build().create().readGraph(
    //   this.getClass.getResourceAsStream("/twitter_graph.json"),
    //   twitterGraph
    // )
    //
    // println("Loaded sample Twitter data")
  }

  override final def afterAll() = {
    // NOTE: uncommend if you want to add data to the GraphSON:
    // import com.tinkerpop.blueprints.util.io.graphson._
    // GraphSONWriter.outputGraph(twitterGraph, "graph_compact.json", GraphSONMode.COMPACT)

    twitterGraph.close()
    println("Shutdown Titan graph")
  }

  case object testSamples {
    import ohnosequences.cosas.types._

    val nousers = user := Container[core.TitanVertex](Iterable())

    def vertices[V <: AnyVertex](v: V): V := TitanVertices =
      v := Container(twitterGraph.query.has("type", v.label).vertices.asScala)

    def edges[E <: AnyEdge](e: E): E := TitanEdges =
      e := Container(twitterGraph.query.has("label", e.label).edges.asScala)

    val users = vertices(user)
    val tweets = vertices(tweet)
    val postEdges = edges(posted)

    val names = name := Container[String](Iterable("@eparejatobes", "@laughedelic", "@evdokim"))
    val ages = age := Container[Int](Iterable(95, 5, 22))
    val times = time := Container[String](Iterable(
      "27.10.2013", "20.3.2013", "19.2.2013", "13.11.2012", "15.2.2014",
      "7.2.2014", "23.2.2012", "7.7.2011", "22.6.2011"
    ))

    val usrs = users.value.values.toList
    val edu = usrs(0)
    val alexey = usrs(1)
    val kim = usrs(2)
  }
  import testSamples._

  test("find bob") {

    import t.evals.categoryStructure._
    val ps = t.evals.propertyStructure(twitterGraph); import ps._
    
    println { evaluate(lookup(user.name))(name("Bob")) }
  }

  ignore("checking evals for the basic structure") {

    import t.evals.categoryStructure._
    import queries.categoryStructure._

    assert( evaluate(q_id)(users) =~= users )
    assert( evaluate(q_comp1)(users) =~= users )
    assert( evaluate(q_comp2)(users) =~= users )
  }

  ignore("checking evals for the property structure") {
    import t.evals.categoryStructure._
    val ps = t.evals.propertyStructure(twitterGraph); import ps._
    import queries.propertyStructure._
    import ohnosequences.cosas.types._

    assert { evaluate(q_getV)(users) =~= ages  }
    assert { evaluate(q_lookupV)(names) =~= users }
    // FIXME: should fail:
    assert { evaluate(lookup(user.name))(name("@evdokim")) =~= users }
    assert { evaluate(q_compV)(names) =~= ages }
    assert { evaluate(q_getE)(postEdges) =~= times }
    assert { evaluate(q_lookupE)(times) =~= postEdges }

    assert { evaluate(get(user.name))(users) =~= names }
    assert { evaluate(q_compE)(postEdges) =~= postEdges }

    assert { evaluate( get(user.name) >=> lookup(user.name) >=> get(user.name))(users) =~= names }
  }

  ignore("checking evals for the tensor structure") {
    import t.evals.categoryStructure._
    val ts = t.evals.tensorStructure(twitterGraph); import ts._
    import queries.tensorStructure._

    assert( evaluate(q_tensor)(users ⊗ users ⊗ users) =~= (users ⊗ users ⊗ users) )
    assert( evaluate(q_dupl)(users ⊗ users) =~= (users ⊗ users ⊗ users) )
    assert( evaluate(q_match)(users ⊗ users) =~= users )
    assert( evaluate(q_comp)(users ⊗ users) =~= users )
  }

  ignore("checking evals for the biproduct structure") {
    import t.evals.categoryStructure._
    import t.evals.biproductStructure._
    import queries.biproductStructure._

    assert( evaluate(q_inj)(tweets) =~= (nousers ⊕ nousers ⊕ tweets) )
    assert( evaluate(q_bip)(users ⊕ users ⊕ tweets) =~= (users ⊕ users ⊕ tweets) )
    assert( evaluate(q_fork)(users ⊕ tweets) =~= (users ⊕ users ⊕ tweets) )
    assert( evaluate(q_merge)(users ⊕ users) =~=
      (user := Container(users.value.values ++ users.value.values))
    )
    assert( evaluate(q_comp)(users ⊕ tweets) =~=
      (tweet := Container(tweets.value.values ++ tweets.value.values))
    )
  }

  ignore("checking evals for the graph structure") {
    import t.evals.categoryStructure._
    import t.evals.graphStructure._
    import queries.graphStructure._

    val repeated = user := Container[core.TitanVertex](
      Iterable(edu, edu, edu, edu, alexey, alexey, kim, kim, kim)
    )

    assert( evaluate(q_outV)(users) =~= tweets )
    assert( evaluate(q_inV)(tweets) =~= repeated )
    assert( evaluate(q_compV)(users) =~= repeated )

    assert( evaluate(q_outE)(users) =~= tweets )
    assert( evaluate(q_inE)(tweets) =~= repeated )
    assert( evaluate(q_compE)(users) =~= repeated )

    assert( evaluate(outV(posted))(users) =~= tweets )
    assert( evaluate(inV(posted))(tweets) =~= repeated )
    assert( evaluate(outV(posted) >=> inV(posted))(users) =~= repeated )

    assert( evaluate(outE(posted) >=> target(posted))(users) =~= tweets )
    assert( evaluate(inE(posted) >=> source(posted))(tweets) =~= repeated )
    assert( evaluate( (outE(posted) >=> target(posted)) >=> (inE(posted) >=> source(posted)) )(users) =~= repeated )

  }

  ignore("checking evals for the predicate structure") {
    import t.evals.categoryStructure._
    import t.evals.predicateStructure._
    import queries.predicateStructure._

    val filtered = Container[core.TitanVertex](Iterable(edu, kim))

    assert( evaluate(q_quant)(users) =~= (pred := filtered) )
    assert( evaluate(q_coerce)(pred := filtered) =~= (user := filtered) )
    assert( evaluate(q_comp)(users) =~= (user := filtered) )
  }

}
