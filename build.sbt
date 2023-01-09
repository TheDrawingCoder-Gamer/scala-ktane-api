val scala3Version = "3.2.1"
val circeVersion = "0.14.1"
lazy val root = project
  .in(file("."))
  .settings(
    organization := "org.bulbyvr",
    name := "ktane-api",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )
