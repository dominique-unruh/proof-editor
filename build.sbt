val os = { val os = sys.props("os.name"); if (os.startsWith("Windows")) "win" else if (os.startsWith("Linux")) "linux" else error("Unrecognized OS: "+os) }
val osbits = os + sys.props("sun.arch.data.model")

val targetOs = "linux"
val targetBits = 64

name := "proof-editor"
version := "0.1"
maintainer := "Dominique Unruh"
organization := "de.unruh"

scalaVersion := "2.11.8"
libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % Test
//libraryDependencies += "org.apache.bcel" % "bcel" % "5.2"
//libraryDependencies += "org.ow2.asm" % "asm" % "5.1"
//libraryDependencies += "commons-io" % "commons-io" % "2.5"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.92-R10" // https://mvnrepository.com/artifact/org.scalafx/scalafx_2.11
//libraryDependencies += "org.apache.xmlgraphics" % "batik-swing" % "1.8"
//libraryDependencies += "org.apache.xmlgraphics" % "xmlgraphics-commons" % "2.1"
//libraryDependencies += "org.jetbrains" % "annotations" % "15.0"
//libraryDependencies += "com.google.code.findbugs" % "jsr305" % "3.0.1"
//libraryDependencies += "org.symcomp" % "openmath" % "1.4.0"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"

resourceDirectory in Compile := baseDirectory.value / "src"
managedResourceDirectories in Compile += baseDirectory.value / "resources"

excludeFilter in unmanagedResources <<= (excludeFilter in unmanagedResources) { _ || "*.java" || "*.scala" || "*~" || "*.orig" }

sourcesInBase := false
scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

enableDownloads

/*
antlr4Settings
antlr4PackageName in Antlr4 := Some("ui.mathview")
sourceDirectory in Antlr4 := baseDirectory.value / "src"
compileOrder in Compile := CompileOrder.Mixed
javaSource in Antlr4 := (sourceManaged in Compile).value
*/

resourceGenerators in Compile += installResources.taskValue

dependencyClasspath in Compile ++= installJars.value
dependencyClasspath in Test ++= installJars.value

envVars := Map("LD_LIBRARY_PATH" -> (baseDirectory.value / "lib" / osbits).getAbsolutePath)

fork in Test := true // Needed because otherwise we try to load the same shared lib in different classloaders during consecutive test runs

JFX.mainClass := Some("testapp.TestApp")

jfxSettings


z3Version := "4.4.1"

cancelable in Global := true

enablePlugins(JavaAppPackaging)

stagingDirectory in Universal := (target in Universal).value / s"stage_$targetOs$targetBits"
NativePackagerKeys.bashScriptExtraDefines += "export LD_LIBRARY_PATH=$lib_dir"
NativePackagerKeys.batScriptExtraDefines += """cd %APP_LIB_DIR%"""

import NativePackagerHelper._

def dirToLibMapping(dir:File) = (dir.*** --- dir) pair {x => relativeTo(dir)(x).map("lib/"+_)}

mappings in Universal ++= { targetOs+targetBits match {
  case "linux64" =>
    installZ3Linux64.value
    Seq(baseDirectory.value / s"lib/linux64" -> "lib")
    dirToLibMapping(baseDirectory.value/"lib"/"linux64")
  case "win32" =>
    installZ3Win32.value
    dirToLibMapping(baseDirectory.value/"lib"/"win32")
  case t =>
    sys.error(s"Don't know how to handle target $t")
}}

//NativePackagerKeys.bashScriptExtraDefines += "export LD_LIBRARY_PATH=$lib_dir"
//NativePackagerKeys.batScriptExtraDefines += """cd %APP_LIB_DIR%"""
//mappings in Universal += baseDirectory.value / "lib/linux64/libz3.so" -> "lib/libz3.so"
//mappings in Universal += baseDirectory.value / "lib/linux64/libz3java.so" -> "lib/libz3java.so"
//mappings in Universal += baseDirectory.value / "lib/win32/libz3.dll" -> "lib/libz3.dll"
//mappings in Universal += baseDirectory.value / "lib/win32/libz3java.dll" -> "lib/libz3java.dll"
//mappings in Universal += baseDirectory.value / "lib/win32/vcomp110.dll" -> "lib/vcomp110.dll"
//enablePlugins(JavaAppPackaging)

