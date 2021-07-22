name := "Messy"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "4.0.4"
)

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
)

scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-deprecation"
)
fork := false

