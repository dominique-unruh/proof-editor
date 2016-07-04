lazy val root = project.in( file(".") ).dependsOn( antlrPlugin )
lazy val antlrPlugin = uri("git://github.com/ihji/sbt-antlr4")

addSbtPlugin("no.vedaadata" %% "sbt-javafx" % "0.7")
