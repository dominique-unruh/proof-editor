name := "proof-editor"

scalaVersion := "2.11.8"
libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"

scalaSource in Compile := baseDirectory.value / "src"
resourceDirectory in Compile := baseDirectory.value / "resources"
sourcesInBase := false

enableDownloads

antlr4Settings
antlr4PackageName in Antlr4 := Some("mathview")
sourceDirectory in Antlr4 := baseDirectory.value / "src"
compileOrder in Compile := CompileOrder.JavaThenScala

resourceGenerators in Compile += installDependencies.taskValue

//compile <<= (compile in Compile) dependsOn installDependencies


unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/ext/jfxrt.jar"))

JFX.mainClass := Some("misc.TestApp")

jfxSettings


//installDependencies := { println("**** Install deps ****"); List() }
//  {
//  println("**** Install deps ****")
//  if (!jqueryFile.exists()) downloadJQuery_
//  if (!mathquillTargetDir.isDirectory()) downloadMathQuill_
//  List(jqueryFile, mathquillTargetDir)
//}
