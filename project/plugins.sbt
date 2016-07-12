lazy val root = project.in( file(".") ).dependsOn( antlrPlugin )
lazy val antlrPlugin = uri("git://github.com/ihji/sbt-antlr4")

addSbtPlugin("no.vedaadata" %% "sbt-javafx" % "0.7")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")
