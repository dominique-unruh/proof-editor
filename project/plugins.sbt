// lazy val root = project.in( file(".") ).dependsOn( antlrPlugin )
// lazy val antlrPlugin = uri("git://github.com/ihji/sbt-antlr4")

addSbtPlugin("com.simplytyped" % "sbt-antlr4" % "0.7.11")

addSbtPlugin("no.vedaadata" %% "sbt-javafx" % "0.7")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.1.4")


addSbtPlugin("org.scala-sbt.plugins" % "sbt-onejar" % "0.8")
