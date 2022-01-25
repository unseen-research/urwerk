val DottyVersion = "3.1.0"
val ReactorVersion = "3.4.9"

lazy val urwerk = project
  .in(file("."))
  .settings(
    name := "Urwerk",
    description := "urwerk scala 3 project",
    version := "0.1.0",

    scalaVersion := DottyVersion,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature"
    ),

    libraryDependencies ++= Seq(
      "io.projectreactor" % "reactor-core" % ReactorVersion % "compile",
      "io.projectreactor" % "reactor-test" % ReactorVersion % "test",
      "org.scala-lang.modules" %% "scala-xml" % "2.0.0" % "compile",
      "info.picocli" % "picocli" % "4.6.2" % "compile",
      "com.outr" %% "scribe" % "3.6.2",
      "com.outr" %% "scribe-slf4j" % "3.6.2",
      "org.wvlet.airframe" %% "airframe-launcher" % "21.12.1",

      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "org.scalatestplus" %% "junit-4-13" % "3.2.9.0" % "test",
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "junit" % "junit" % "4.13" % "test",
      "com.github.tomakehurst" % "wiremock-jre8" % "2.27.2" % "test"
    )
  )
