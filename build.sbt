name := "fusion"

organization := "org.improving"

version := "0.1.1-SNAPSHOT"

scalaVersion := "2.10.1"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

libraryDependencies <<= (scalaVersion)(sv =>
  Seq(
    "org.scala-lang" % "scala-compiler" % sv,
    "com.novocode" % "junit-interface" % "0.8" % "test"
  )
)
