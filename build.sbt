name := "Messy"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "com.microsoft.z3" % "java-jar" % "4.8.8"
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

