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

lazy val testing = project
  .in(file("modules/testing/shared"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core)
  .settings(
    name := "cassiphone-testing",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-numbers-testing" % "0.14.2",
      "org.scalacheck" %%% "scalacheck" % "1.16.0",
      "org.typelevel" %%% "discipline-core" % "1.5.1",
      "org.typelevel" %%% "cats-laws" % "2.8.0"
    )
  )

lazy val tests = project
  .in(file("modules/tests/shared"))
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin)
  .dependsOn(core, testing)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit-scalacheck" % "1.0.0-M6",
      "org.typelevel" %%% "discipline-munit" % "2.0.0-M3",
      "io.github.cquiroz" %%% "scala-java-time" % "2.4.0"
    )
  )
