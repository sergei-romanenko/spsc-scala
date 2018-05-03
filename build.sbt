scalaVersion := "2.12.6"

name := "spsc-lite"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

scalacOptions ++= Seq("-deprecation", "-feature")

//testOptions in Test += Tests.Argument("-oD")

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

logBuffered in Test := false

parallelExecution in Test := false
