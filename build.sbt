scalaVersion := "2.12.6"

name := "spsc-scala"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.typelevel" %% "paiges-core" % "0.2.0"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

testOptions in Test += Tests.Argument(args= "-oD")

logBuffered in Test := false

parallelExecution in Test := false
