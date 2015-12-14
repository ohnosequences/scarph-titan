Nice.scalaProject

name          := "scarph-titan"
description   := "Scarph evaluators for TitanDB"
organization  := "ohnosequences"
bucketSuffix  := "era7.com"

scalaVersion        := "2.11.7"
crossScalaVersions  := Seq("2.10.5")

libraryDependencies ++= Seq(
  "ohnosequences"           %% "scarph"           % "0.4.0-SNAPSHOT",
  "com.thinkaurelius.titan" %  "titan-core"       % "0.5.2",
  "com.thinkaurelius.titan" %  "titan-berkeleyje" % "0.5.2",
  "org.scalatest"           %% "scalatest"        % "2.2.5" % Test,
  "org.slf4j"               %  "slf4j-nop"        % "1.7.5" % Test,
  "ohnosequences"           %% "scarph"           % "0.4.0-SNAPSHOT" % Test classifier "tests"
  // ^ getting rid of the annoying warning about logging ^
)

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")

// no name hashing, funny stuff happens
incOptions := incOptions.value.withNameHashing(false)

wartremoverExcluded ++= Seq(
  baseDirectory.value / "src/main/scala/ohnosequences/scarph/impl/titan/types.scala"
)

scalacOptions ++= Seq("-optimise", "-Yinline", "-Yinline-warnings")
