import com.simplytyped.Antlr4Plugin

name := "proof-editor"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"

antlr4Settings
antlr4GenVisitor in Antlr4 := true
antlr4GenListener in Antlr4 := false
antlr4PackageName in Antlr4 := Some("webview")

//scalaSource in Compile := baseDirectory.value / "src"
//scalaSource in Compile += file("target/scala-2.11/src_managed/main/antlr4")

//javaSource in Compile := file("target/scala-2.11/src_managed/main/antlr4")

//antlr4Generate := Seq(file("src/mathview/MQLatex.g4x"))
//sourceDirectory in Antlr4 := file("src")

//lazy val hello = taskKey[Unit]("Prints 'Hello World'")
//hello := {
//  println("hello world!" + antlr4Generate.value ) }
