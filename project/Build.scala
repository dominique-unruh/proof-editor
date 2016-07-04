//lazy val root = (project in file(".")).dependsOn(antlrPlugin)
//lazy val antlrPlugin = uri("https://github.com/ihji/sbt-antlr4.git#17bb8163e6c7053ed160275ec74b3ff493fd4265")
//import sbt._
//object PluginDef extends Build {
//  override lazy val projects = Seq(root)
//  lazy val root = Project("plugins", file(".")) dependsOn( shPlugin )
//  lazy val shPlugin = uri("https://github.com/ihji/sbt-antlr4.git#17bb8163e6c7053ed160275ec74b3ff493fd4265")
//}