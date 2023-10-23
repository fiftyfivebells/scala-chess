import Dependencies._

ThisBuild / organization := "dev.ffb"
ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "scala-chess",
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    libraryDependencies ++= Seq(
      `zio`,
      `zio-test`,
      `zio-test-sbt`,
      `zio-http`,
      `zio-http-test`,
      `zio-json`
    )
  )
