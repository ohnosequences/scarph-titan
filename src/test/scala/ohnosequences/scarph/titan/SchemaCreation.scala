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
import util.{ Success, Failure, Try }
import reflect.ClassTag

class SchemaCreation extends org.scalatest.FunSuite {

  test("create all types") {

    val tGraph =
      core.TitanFactory.open("inmemory")

    val vertices =
      twitter.vertices.toList

    val edges =
      twitter.edges.toList

    val properties =
      twitter.properties.toList

    val createdTypesT =
      tGraph.withManager { mgmt =>
        ///////////////////////////////////////////// vertices
        println { s"creating vertices: ${vertices}" }
        val vlbls =
          vertices.map(mgmt.addVertexLabel)
        // println{ s"added vertex labels: ${vlbls}" }
        ///////////////////////////////////////////// edges
        println { s"creating edges: ${edges}" }
        val elbls =
          edges.map(mgmt.addEdgeLabel)
        // println{ s"added edge labels: ${edgeLabels}" }
        ///////////////////////////////////////////// properties
        println { s"creating properties: ${properties}" }
        val pks =
          properties.map(mgmt.addPropertyKey)
        // println{ s"added property keys: ${propertyKeys}" }
        (vlbls, elbls, pks)
      }

    createdTypesT match {

      case Success((vertexLabels, edgeLabels, propertyKeys)) =>
        tGraph.withManager { mgmt =>
          assert { Set(vertexLabels)  === Set( vertices.map(v => mgmt.getVertexLabel(v.label)) ) }
          assert { Set(propertyKeys)  === Set( properties.map(p => mgmt.getPropertyKey(p.label)) ) }
          assert { Set(edgeLabels)    === Set( edges.map(e => mgmt.getEdgeLabel(e.label)) ) }
        }

      case Failure(err) => println(err); fail("Error creating types")

    }
  }
}
