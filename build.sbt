name := "proof-editor"

scalaVersion := "2.11.8"
libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % Test
libraryDependencies += "org.apache.bcel" % "bcel" % "5.2"
libraryDependencies += "org.ow2.asm" % "asm" % "5.1"
libraryDependencies += "commons-io" % "commons-io" % "2.5"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
resourceDirectory in Compile := baseDirectory.value / "resources"
unmanagedResourceDirectories in Compile += baseDirectory.value / "src" // TODO: remove (only non-source files)
//unmanagedResourceDirectories in Compile += baseDirectory.value / "resources"
sourcesInBase := false

enableDownloads

antlr4Settings
antlr4PackageName in Antlr4 := Some("mathview")
sourceDirectory in Antlr4 := baseDirectory.value / "src"
compileOrder in Compile := CompileOrder.JavaThenScala

resourceGenerators in Compile += installDependencies.taskValue

//compile <<= (compile in Compile) dependsOn installDependencies

//val _ = println(System.getenv("JAVA_HOME"))
//unmanagedJars in Compile += file(System.getenv("JAVA_HOME")) / "/jre/lib/ext/jfxrt.jar"
unmanagedJars in Compile += file("/usr/lib64/z3/com.microsoft.z3.jar")
unmanagedResources in Compile += file("/usr/lib64/libz3.so.0")
unmanagedResources in Compile += file("/usr/lib64/z3/libz3java.so")

fork in Test := true // Needed because otherwise we try to load the same shared lib in different classloaders during consecutive test runs

JFX.mainClass := Some("misc.TestApp")

jfxSettings


//installDependencies := { println("**** Install deps ****"); List() }
//  {
//  println("**** Install deps ****")
//  if (!jqueryFile.exists()) downloadJQuery_
//  if (!mathquillTargetDir.isDirectory()) downloadMathQuill_
//  List(jqueryFile, mathquillTargetDir)
//}
