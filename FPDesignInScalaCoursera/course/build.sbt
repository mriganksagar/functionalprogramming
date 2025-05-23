val scala3Version = "3.6.4"

lazy val course = project
  .in(file("."))
  .settings(
    name := "course",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
