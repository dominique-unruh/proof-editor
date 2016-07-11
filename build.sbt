

name := "proof-editor"

scalaVersion := "2.11.8"
libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % Test
libraryDependencies += "org.apache.bcel" % "bcel" % "5.2"
libraryDependencies += "org.ow2.asm" % "asm" % "5.1"
libraryDependencies += "commons-io" % "commons-io" % "2.5"
//libraryDependencies += "com.storm-enroute" % "coroutines_2.11" % "0.4" // TODO: "0.6" instead

scalaSource in Compile := baseDirectory.value / "src"

resourceDirectory in Compile := baseDirectory.value / "src"
managedResourceDirectories in Compile += baseDirectory.value / "resources"

excludeFilter in unmanagedResources <<= (excludeFilter in unmanagedResources) { _ || "*.scala" || "*~" }
//sbt.Defaults

sourcesInBase := false
scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

enableDownloads

antlr4Settings
antlr4PackageName in Antlr4 := Some("mathview")
sourceDirectory in Antlr4 := baseDirectory.value / "src"
compileOrder in Compile := CompileOrder.Mixed

resourceGenerators in Compile += installDependencies.taskValue

//compile <<= (compile in Compile) dependsOn installDependencies

//val _ = println(System.getenv("JAVA_HOME"))
//unmanagedJars in Compile += file(System.getenv("JAVA_HOME")) / "/jre/lib/ext/jfxrt.jar"
unmanagedJars in Compile += findFile("com.microsoft.z3.jar", file("/usr/lib64/z3"), file("/opt/z3/bin"))
unmanagedResources in Compile += findFile(List("libz3.so","libz3.so.0"), file("/usr/lib64"), file("/opt/z3/bin"))
unmanagedResources in Compile += findFile("libz3java.so", file("/usr/lib64/z3"), file("/opt/z3/bin"))

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
