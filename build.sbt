name := "Messy"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "org.sosy-lab" % "javasmt-solver-z3" % "z3-4.4.1-788-g8df145d"
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

