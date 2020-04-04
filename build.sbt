name := "sudoku"
version := "0.1"

scalaVersion := "2.13.1"

val scalaTestVersion = "3.1.1"

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % "0.9.12",
  "org.scalactic" %% "scalactic" % scalaTestVersion, // TODO: identify
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1",
)

