name := "S99,Scala 99 Problem"

version := "1.0"

scalaVersion := "2.12.3"

scalacOptions ++= 
  Seq("-deprecation",
      "-feature",
      "-target:jvm-1.8",
      "-unchecked",
      "-Ypartial-unification")

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF"

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.0.4"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
 
)

coverageEnabled := true

