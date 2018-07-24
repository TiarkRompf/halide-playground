name := "tensor-playground"

version := "0.0.1"

scalaVersion := "2.12.2"

resolvers += Resolver.sonatypeRepo("snapshots")

// libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "1.0.0-SNAPSHOT"

// libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"

// libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.11.2"

// libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5"

scalacOptions += "-deprecation"
