package ohnosequences.scarph.impl.titan.test

import com.thinkaurelius.titan.core


class TitanSuite extends org.scalatest.FunSuite with org.scalatest.BeforeAndAfterAll {

  import ohnosequences.{ scarph => s }
  import s.objects._, s.evals._, s.morphisms._
  import s.syntax.objects._, s.syntax.morphisms._
  import s.test._, twitter._

  import ohnosequences.scarph.impl.{ titan => t }
  import t.evals._, t.types._, t.rewrites._, t.syntax._

  import java.io.File

  // val graphLocation = new File("/tmp/titanTest")
  // var twitterGraph: titan.TitanGraph = null
  val twitterGraph = core.TitanFactory.open("inmemory")

  override final def beforeAll() {

    /*
    def cleanDir(f: File) {
      if (f.isDirectory) f.listFiles.foreach(cleanDir(_))
      else { println(f.toString); f.delete }
    }
    cleanDir(graphLocation)
    */

    // twitterGraph = TitanFactory.open("berkeleyje:" + graphLocation.getAbsolutePath)

    println("Created Titan graph")

    import ohnosequences.scarph.impl.titan.titanSchema._
    import ohnosequences.scarph.test._

    // twitterGraph.createSchema(twitter)

    import com.tinkerpop.blueprints.util.io.graphson._
    GraphSONReader.inputGraph(twitterGraph, this.getClass.getResource("/twitter_graph.json").getPath)

    println("Loaded sample Twitter data")
  }

  override final def afterAll() {
    // NOTE: uncommend if you want to add data to the GraphSON:
    // import com.tinkerpop.blueprints.util.io.graphson._
    // GraphSONWriter.outputGraph(twitterGraph, "graph_compact.json", GraphSONMode.COMPACT)

    twitterGraph.shutdown
    println("Shutdown Titan graph")
  }

  //val impl = ohnosequences.scarph.impl.titan.evals.all(twitterGraph); import impl._*/

  val nouser = user := Container[core.TitanVertex](Seq())

  test("checking evals for the basic structure") {
    import t.evals.categoryStructure._
    import queries.categoryStructure._

    assert{ eval(q_id)(nouser) == nouser }
    assert{ eval(q_comp1)(nouser) == nouser }
    assert{ eval(q_comp2)(nouser) == nouser }
  }

  test("checking evals for the tensor structure") {
    import t.evals.categoryStructure._
    val ts = t.evals.tensorStructure(twitterGraph); import ts._
    import queries.tensorStructure._

    assert{ eval(q_tensor)(nouser ⊗ nouser ⊗ nouser) == nouser ⊗ nouser ⊗ nouser }
    assert{ eval(q_dupl)(nouser ⊗ nouser) == nouser ⊗ nouser ⊗ nouser }
    assert{ eval(q_match)(nouser ⊗ nouser) == nouser }
    assert{ eval(q_comp)(nouser ⊗ nouser) == nouser }
  }

/*
  test("checking evals for the biproduct structure") {
    import t.evals.categoryStructure._
    import t.evals.biproductStructure._
    import queries.biproductStructure._

    assert{ eval(q_inj)(dt) == du ⊕ du ⊕ dt }
    assert{ eval(q_bip)(du ⊕ du ⊕ dt) == du ⊕ du ⊕ dt }
    assert{ eval(q_fork)(du ⊕ dt) == du ⊕ du ⊕ dt }
    assert{ eval(q_merge)(du ⊕ du) == du }
    assert{ eval(q_comp)(du ⊕ dt) == dt }
  }

  test("checking evals for the graph structure") {
    import t.evals.categoryStructure._
    import t.evals.graphStructure._
    import queries.graphStructure._

    assert{ eval(q_outV)(du) == dt }
    assert{ eval(q_inV)(dt) == du }
    assert{ eval(q_compV)(du) == du }

    assert{ eval(q_outE)(du) == dt }
    assert{ eval(q_inE)(dt) == du }
    assert{ eval(q_compE)(du) == du }
  }

  test("checking evals for the property structure") {
    import t.evals.categoryStructure._
    import t.evals.propertyStructure._
    import queries.propertyStructure._

    // FIXME: this works if you put dnames on the right (no tag/type parameter check)
    assert{ eval(q_getV)(du) == dages }
    assert{ eval(q_lookupV)(dnames) == du }
    assert{ eval(q_compV)(dnames) == dages }

    assert{ eval(q_getE)(dp) == dtimes }
    assert{ eval(q_lookupE)(dtimes) == dp }
    assert{ eval(q_compE)(dp) == dp }
  }
*/

}
