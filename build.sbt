name := "sudoku"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "eu.timepit" %% "refined" % "0.9.12"

val Test = ""
val scalaTestVersion = "3.1.1"
libraryDependencies += "org.scalactic" %% "scalactic" % scalaTestVersion // TODO: identify
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1"
