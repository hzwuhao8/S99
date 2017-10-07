name := "S99,Scala 99 Problem"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= 
  Seq("-deprecation",
      "-feature",
      "-target:jvm-1.8",
      "-unchecked")

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "2.2.1"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "org.pegdown"    %  "pegdown"    % "1.4.2"  % "test"
)

