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
