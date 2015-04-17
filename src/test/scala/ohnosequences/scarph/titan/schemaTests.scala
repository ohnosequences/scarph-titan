package ohnosequences.scarph.impl.titan.test

import ohnosequences.scarph.schemas._
import ohnosequences.scarph.graphTypes._
import com.thinkaurelius.titan.{ core => titan }
import titan.TitanGraph
import titan.schema.TitanManagement

import scala.reflect.runtime.universe._

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


  import java.io.File
  import com.thinkaurelius.titan.core._
  import com.thinkaurelius.titan.core.Multiplicity._
  import com.thinkaurelius.titan.core.schema._

  val graphLocation = new File("/tmp/titanTest")
  var g: TitanGraph = null

  test("properties and TypeTags") {

    import ohnosequences.scarph.impl.titan.titanSchema._
    import ohnosequences.scarph.test._

    twitter createTypesIn g
  }





  override final def beforeAll() {

    g = TitanFactory.open("berkeleyje:" + graphLocation.getAbsolutePath)

    def cleanDir(f: File) {
      if (f.isDirectory) f.listFiles.foreach(cleanDir(_))
      else { println(f.toString); f.delete }
    }
    cleanDir(graphLocation)
    g = TitanFactory.open("berkeleyje:" + graphLocation.getAbsolutePath)

    println("Created Titan graph")
  }

  override final def afterAll() {
    if(g != null) {
      g.shutdown
      println("Shutdown Titan graph")
    }
  }

  test("foo") {
    import ohnosequences.scarph._, evals._, morphisms._, syntax.morphisms._
    import ohnosequences.scarph.test._, twitter._
    import ohnosequences.scarph.impl.titan.evals._
    import ohnosequences.scarph.impl.titan.implementations._
    import ohnosequences.scarph.impl.titan.types._

    object titanTwitterEvals extends DefaultTitanEvals { val graph = g }
    import titanTwitterEvals._


    val query = lookup(user.name) //.get(user.age)

    println(query.evalOn(name := (Seq("@laughedelic"): Container[String]))(eval_lookup(TitanPropertyVertexImpl[user.name.type](g))))
  }

}
