lazy val commonSettings = Seq(
  organization := "ru.yandex.money",
  name := "auction_bot",
  version := "0.1.0",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.google.code.gson" % "gson" % "2.4",
      "io.vertx" % "vertx-core" % "3.2.1",
      "io.vertx" % "vertx-web" % "3.2.1",
      "io.vertx" % "vertx-codegen" % "3.2.1",
      "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4",
      "com.squareup.okhttp" % "okhttp" % "2.5.0",
      "io.netty" % "netty-all" % "4.0.33.Final",
      "joda-time" % "joda-time" % "2.8.2",
      "org.json4s" %% "json4s-native" % "3.3.0",
      "com.typesafe" % "config" % "1.3.0",
      "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.1.0",
      "org.scala-lang" % "scala-reflect" % "2.11.1",
      "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"
    )
  )
