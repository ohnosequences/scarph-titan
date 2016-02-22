Nice.scalaProject

name          := "scarph-titan"
description   := "Scarph evaluators for TitanDB"
organization  := "ohnosequences"
bucketSuffix  := "era7.com"

scalaVersion        := "2.11.7"

val titanVersion = "0.5.4"
val scarphVersion = "0.3.1"

libraryDependencies ++= Seq(
  "ohnosequences"           %% "cosas"            % "0.8.0",
  "com.thinkaurelius.titan" %  "titan-core"       % titanVersion,
  "com.thinkaurelius.titan" %  "titan-berkeleyje" % titanVersion % Test,
  "ohnosequences"           %% "scarph"           % scarphVersion,
  "ohnosequences"           %% "scarph"           % scarphVersion % Test classifier "tests",
  "org.scalatest"           %% "scalatest"        % "2.2.6"       % Test,
  "org.slf4j"               %  "slf4j-nop"        % "1.7.5"       % Test
  // ^ getting rid of the annoying warning about logging ^
)

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")

// no name hashing, funny stuff happens
// incOptions := incOptions.value.withNameHashing(false)
// scalacOptions ++= Seq("-optimise", "-Yinline", "-Yinline-warnings")

// FIXME
wartremoverWarnings ++= Warts.all
wartremoverErrors in (Compile, compile) := Seq()
