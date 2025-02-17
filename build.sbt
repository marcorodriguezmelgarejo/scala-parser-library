name := "scala"

version := "1"

organization := "edu.ar.utn.tadp"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  "org.scalactic" %% "scalactic" % "3.2.15"
)

lazy val modeloMusica = ProjectRef(uri("https://github.com/tadp-utn-frba/tadparser-musica-modelo.git#main"),"tadparser-musica-modelo")

lazy val root = (project in file("."))
  .dependsOn(modeloMusica)
