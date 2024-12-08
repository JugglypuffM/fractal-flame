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
)