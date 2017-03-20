name          := "scarph-titan"
description   := "Scarph evaluators for TitanDB"
organization  := "ohnosequences"
bucketSuffix  := "era7.com"

val scarphVersion = "0.3.1-32-gca14792"
val titanVersion  = "1.0.0"

libraryDependencies ++= Seq(
  "ohnosequences"           %% "cosas"            % "0.8.0",
  "com.thinkaurelius.titan" %  "titan-core"       % titanVersion,
  "com.thinkaurelius.titan" %  "titan-berkeleyje" % titanVersion % Test,
  "ohnosequences"           %% "scarph"           % scarphVersion,
  "ohnosequences"           %% "scarph"           % scarphVersion % Test classifier "tests",
  "org.scalatest"           %% "scalatest"        % "3.0.1"       % Test,
  "org.slf4j"               %  "slf4j-nop"        % "1.7.5"       % Test
  // ^ getting rid of the annoying warning about logging ^
)

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")

// no name hashing, funny stuff happens
// incOptions := incOptions.value.withNameHashing(false)
// scalacOptions ++= Seq("-optimise", "-Yinline", "-Yinline-warnings")

// FIXME
// wartremoverWarnings ++= Warts.all
wartremoverErrors in (Compile, compile) := Seq()
wartremoverErrors in (Test,    compile) := Seq()



// libraryDependencies += "com.tinkerpop.blueprints" % "blueprints-core" % "2.5.0"
// conflictManager := ConflictManager.default
// libraryDependencies += "com.lihaoyi" % "ammonite" % "0.8.2" cross CrossVersion.full
// initialCommands in (Compile, console) := """ammonite.Main().run()"""
