import scala.io.StdIn

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

lazy val root = (project in file("."))
  .settings(
    name := "min-triangle-path"
  )

val zioVersion = "2.0.15"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion
)
