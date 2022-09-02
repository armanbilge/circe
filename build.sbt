ThisBuild / tlBaseVersion := "0.15"

ThisBuild / organization := "com.armanbilge"
ThisBuild / organizationName := "Arman Bilge"
ThisBuild / developers += tlGitHubDev("armanbilge", "Arman Bilge")
ThisBuild / startYear := Some(2022)
ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / crossScalaVersions := Seq("3.1.3")

lazy val root = project.in(file(".")).aggregate(core).enablePlugins(NoPublishPlugin)

lazy val core = project
  .in(file("modules/core/shared"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "cassiphone-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.8.0",
      "io.circe" %%% "circe-numbers" % "0.14.2"
    ),
    Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.gen).taskValue
  )
