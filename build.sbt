name := "fusion"

organization := "org.improving"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0-SNAPSHOT"

//scalaHome := Some(file("/scala/inst/3"))

//retrieveManaged := true

resolvers += Opts.resolver.sonatypeSnapshots

libraryDependencies <<= (scalaVersion)(sv => 
  Seq(
    "org.scala-lang" % "scala-compiler" % sv,
    "org.scala-lang" % "scala-reflect" % sv
  )
)
