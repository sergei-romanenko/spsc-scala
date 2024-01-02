scalaVersion := "2.12.18"

name := "spsc-scala"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.typelevel" %% "paiges-core" % "0.2.0"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

Test / testOptions += Tests.Argument(args= "-oD")

Test / logBuffered := false

Test / parallelExecution := false
