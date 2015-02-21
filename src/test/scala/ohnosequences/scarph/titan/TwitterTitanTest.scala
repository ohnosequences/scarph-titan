package ohnosequences.scarph.test.impl

import com.thinkaurelius.titan.core.{ TitanFactory, TitanGraph, TitanVertex, TitanEdge }
import com.thinkaurelius.titan.core.PropertyKey
import com.thinkaurelius.titan.core.schema.TitanManagement

import ohnosequences.cosas._, types._

import ohnosequences.{ scarph => s }
import s.graphTypes._, s.steps._, s.containers._, s.combinators._, s.indexes._, s.syntax
import s.syntax._, conditions._, predicates._, paths._, graphTypes._
import s.impl, impl.titan.schema._, impl.titan.predicates._, impl.titan.evals._
import s.test.Twitter._


trait AnyTitanTestSuite 
      extends org.scalatest.FunSuite 
      with org.scalatest.BeforeAndAfterAll 
      with ohnosequences.scarph.test.ScalazEquality {

  val g: TitanGraph = TitanFactory.open("inmemory")

  val titanTwitter = twitter := g

  override def beforeAll() {
    titanTwitter.createSchema

    // loading data from a prepared GraphSON file
    import com.tinkerpop.blueprints.util.io.graphson._
    GraphSONReader.inputGraph(g, getClass.getResource("/twitter_graph.json").getPath)
  }

  override def afterAll() {
    g.shutdown

    // // NOTE: uncommend if you want to add data to the GraphSON:
    // import com.tinkerpop.blueprints.util.io.graphson._
    // GraphSONWriter.outputGraph(g, "graph_compact.json", GraphSONMode.COMPACT)
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

class TitanTestSuite extends AnyTitanTestSuite {

  test("check schema keys/labels") {

    val errors = titanTwitter.checkSchema
    if (errors.nonEmpty) errors.foreach{ err => info(err.msg) }
    assert{ errors.isEmpty }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////

  object TestContext {

    // predicates for quering vertices
    val askEdu = user ? (name === "@eparejatobes")
    val askAlexey = user ? (name === "@laughedelic")
    val askKim = user ? (name === "@evdokim")

    val askTweet = tweet ? (text === "back to twitter :)")

    // predicates for quering edges
    val askPost = posted ? (time === "13.11.2012")

    // prepared test queries (they can be reused for different tests)
    val userName = Get(name)
    val postAuthor = Source(posted)
    val postAuthorName = postAuthor >=> userName

    val edu  = user := twitter.query(askEdu).evalOn( titanTwitter ).value.head
    val alexey  = user := twitter.query(askAlexey).evalOn( titanTwitter ).value.head
    val kim  = user := twitter.query(askKim).evalOn( titanTwitter ).value.head
    val post = posted := twitter.query(askPost).evalOn( titanTwitter ).value.head
    val twt  = tweet := twitter.query(askTweet).evalOn( titanTwitter ).value.head
  }

  test("check what we got from the index queries") {
    import TestContext._

    /* Evaluating steps: */
    assert{ Get(name).evalOn( edu ) == name("@eparejatobes") }
    assert{ Source(posted).evalOn( post ) == edu }

    /* Composing steps: */
    val posterName = Source(posted) >=> Get(name)
    assert{ posterName.evalOn( post ) == (name := "@eparejatobes") }

    assertResult( OneOrNone.of(user) := None ){ 
      twitter.query(userByNameAndAge, user ? (name === "@eparejatobes") and (age === 5))
        .evalOn( titanTwitter )
    }
    assertResult( OneOrNone.of(user) := Some(edu.value) ){ 
      twitter.query(userByNameAndAge, user ? (age === 95) and (name === "@eparejatobes"))
        .evalOn( titanTwitter )
    }

    assert{ userName.evalOn( edu ) == name("@eparejatobes") }
    assert{ postAuthor.evalOn( post ) == edu }

    assert{ postAuthorName.evalOn( post ) == name("@eparejatobes") }
  }

  test("cool queries dsl") {
    import TestContext._

    // element op:
    val userName = user.get(name)
    assert{ userName.evalOn( edu ) == name("@eparejatobes") }

    // edge op:
    val posterName = posted.src.get(name)
    assert{ posterName.evalOn( post ) == name("@eparejatobes") }

    // vertex op:
    val friendsPosts =
      user.outE( follows )
          .flatMap( follows.tgt )
          .flatMap( user.outE(posted)
          .flatMap( posted.tgt ) )
    assert{ friendsPosts.out == ManyOrNone.of(tweet) }

    // testing vertex query
    val vertexQuery = user.outE(posted ? (time === "27.10.2013")).map( posted.get(url) )
    // NOTE: scalaz equality doesn understand that these are the same types, so there are just two simple checks:
    implicitly[ vertexQuery.Out ≃ ManyOrNone.Of[url.type] ]
    assert{ vertexQuery.out == ManyOrNone.of(url) }
    assert{ vertexQuery.evalOn( edu ) == (ManyOrNone.of(url) := Stream("https://twitter.com/eparejatobes/status/394430900051927041")) }
  }

  test("evaluating MapOver") {
    import TestContext._

    assertResult( OneOrNone.of(user) := (Option("@eparejatobes")) ){ 
      MapOver(Get(name), OneOrNone).evalOn( 
        OneOrNone.of(user) := (Option(edu.value))
      )
    }

  }

  test("checking combination of Composition and MapOver") {
    import TestContext._

    assertResult( ManyOrNone.of(user) := Stream("@laughedelic", "@evdokim") ){ 
      val q = user.outE(follows)
      (q >=> MapOver(follows.tgt.get(name), q.out.container)).evalOn( edu )
    }

    assertResult( ManyOrNone.of(user) := (Stream("@laughedelic", "@evdokim")) ){ 
      user.outE(follows).map( follows.tgt.get(name) ).evalOn( edu )
    }

    assertResult( ManyOrNone.of(age) := Stream(5, 22) ){ 
      twitter.query(user ? (age < 80)).map( Get(age) ).evalOn( titanTwitter )
    }
    assertResult( ManyOrNone.of(age) := Stream(22) ){ 
      twitter.query(user ? (age < 80) and (age > 10)).map( Get(age) ).evalOn( titanTwitter)
    }
    assertResult( ManyOrNone.of(age) := Stream(5, 22) ){ 
      twitter.query(user ? (age between (3, 25))).map( Get(age) ).evalOn( titanTwitter)
    }

    // println(any(user).label)
    // println((user ? (age < 80)).label)
    // println((user ? (age < 80) and (age > 10)).label)
    // println((user ? (age between (3, 25))).label)

  }

  test("flattening after double map") {
    import TestContext._

    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      Flatten(
        twitter.query(askEdu)
          .map( user.outV(follows) )
      ).map( user.get(name) )
      .evalOn( titanTwitter )
    }

    // Same with .flatten syntax:
    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      twitter.query(askEdu)
        .map( user.outV(follows) )
        .flatten
        .map( user.get(name) )
      .evalOn( titanTwitter )
    }

    // Same with .flatMap syntax:
    assertResult( (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ){ 
      twitter.query(askEdu)
        .flatMap( user.outV(follows) )
        .map( user.get(name) )
      .evalOn( titanTwitter )
    }

    // Flattening with ManyOrNone × ExactlyOne:
    val followersNames = user
      .outV( follows )
      .map( user.get(name) )

    implicitly[ followersNames.Out ≃ ManyOrNone.Of[ExactlyOne.Of[name.type]] ]
    assert{ followersNames.out == ManyOrNone.of(ExactlyOne.of(name)) }

    implicitly[ followersNames.Out ≃ ManyOrNone.Of[name.type] ]
    assert{ followersNames.out == ManyOrNone.of(name) }

    assertResult( ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim") ){ 
      followersNames.flatten.evalOn( edu )
    }

    // Flattening ExactlyOne × ExactlyOne:
    val posterName = posted
      .src
      .map( user.get(name) )

    assert{ posterName.out == name }

    assertResult( name := "@eparejatobes" ){ 
      posterName.evalOn( post )
    }

    assertResult( name := "@eparejatobes" ){ 
      posterName.flatten.evalOn( post )
    }

    // TODO: test all container combinations

  }

  test("type-safe equality for labeled values") {

    assertTypeError("""
      (ManyOrNone.of(user) := "hola") === (user := "hola")
    """)

    assertTypeError("""
      name("hola") === text("hola")
    """)

    assertTypeError("""
      (ManyOrNone.of(user) := "yuhuu") === (ManyOrNone.of(user) := 12)
    """)
  }

  test("parallel combinator") {
    import TestContext._

    // friends' names
    val friendsNames = user
      .outV(follows)
      .map( user.get(name) )

    // friends' ages
    val friendsAges = user
      .outV(follows)
      .map( user.get(age) )

    val result =
      (ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim")) ⊗
      (ManyOrNone.of(age) := Stream(5, 22)) 

    // using explicit Par
    assertResult(result) {
      (friendsNames ⊗ friendsAges).evalOn( edu ⊗ edu )
    }

    // now using forkMap:
    assertResult(result) {
      user
        .outV(follows)
        .forkMap( user.get(name) ⊗ user.get(age) )
      .evalOn( edu )
    }

    // now testing just fork:
    assertResult( (name := "@eparejatobes") ⊗ (age := 95) ) {
      tweet
        .inV(posted)  // getting exactly one author
        .fork( user.get(name) ⊗ user.get(age) )
      .evalOn( twt )
    }
  }

  test("choice combinator") {
    import TestContext._

    // friends' names
    val friendsNames = user
      .outV(follows)
      .map( user.get(name) )

    // friends' ages
    val friendsAges = user
      .outV(follows)
      .map( user.get(age) )

    // just choosing left or right:
    assertResult( ManyOrNone.of(name) := Stream("@laughedelic", "@evdokim") ) {
      user.left(friendsNames ⊗ friendsAges).evalOn( edu )
    }

    assertResult( ManyOrNone.of(age) := Stream(5, 22) ) {
      user.right(friendsNames ⊗ friendsAges).evalOn( edu )
    }

  }

  test("merging results") {
    import TestContext._

    assertResult(ManyOrNone.of(user) := Stream(alexey.value, kim.value, alexey.value, kim.value)) {
      ( user.inV(follows) ⊗ user.outV(follows) )
        .merge
      .evalOn( edu ⊗ edu )
    }
  }

}
