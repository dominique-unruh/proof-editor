//addSbtPlugin("net.ground5hark.sbt" % "sbt-concat" % "0.1.9")
//resolvers += Resolver.sonatypeRepo("releases")
//lazy val root = (project in file(".")).enablePlugins(SbtConcat)
//pipelineStages := Seq(concat)


lazy val root = project.in( file(".") ).dependsOn( antlrPlugin )
//lazy val assemblyPlugin = uri("git://github.com/ground5hark/sbt-concat")

//lazy val root = (project in file(".")).dependsOn(antlrPlugin)
//lazy val antlrPlugin = uri("https://github.com/ihji/sbt-antlr4.git#17bb8163e6c7053ed160275ec74b3ff493fd4265")
lazy val antlrPlugin = uri("git://github.com/ihji/sbt-antlr4")
