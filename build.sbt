name := "Leon"

version := "2.0"

organization := "ch.epfl.lara"

scalaVersion := "2.9.2"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

javacOptions += "-Xlint:unchecked"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.9.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

if(System.getProperty("sun.arch.data.model") == "64") {
  unmanagedBase <<= baseDirectory { base => base / "unmanaged" / "64" }
} else {
  unmanagedBase <<= baseDirectory { base => base / "unmanaged" / "32" }
}

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M3" % "test"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.4"

fork in run := true

fork in test := true

EclipseKeys.skipParents in ThisBuild := false

libraryDependencies += "com.dongxiguo" %% "zero-log" % "0.1.2"
