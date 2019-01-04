lazy val `upem-partiel-sujet2` = (project in file("."))
  .settings(
    organization := "fr.upem",
    name := "upem-partiel-sujet2",
    scalaVersion := "2.12.8",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies ++= List(
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )
