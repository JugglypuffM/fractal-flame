import Dependencies.{Versions => _, _}

lazy val samples = project
  .settings(
    name := "samples",
    scalaVersion := Versions.scala3,
    libraryDependencies ++= Seq(scalaTest, scalastic)
  )

name := "fractal-flame"

version := "1.0"

scalaVersion := "3.3.3"

scalaVersion := Versions.scala3
libraryDependencies ++= Seq(scalaTest, scalastic)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.12.0",
  "org.typelevel" %% "cats-effect" % "3.5.4",
  "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test,

  "org.scalamock" %% "scalamock" % "6.0.0" % Test,

  "co.fs2" %% "fs2-core" % "3.10.2",
  "co.fs2" %% "fs2-io" % "3.10.2",

  "com.github.scopt" %% "scopt" % "4.1.0",
)