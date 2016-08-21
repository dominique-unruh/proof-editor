import com.typesafe.sbt.packager.MappingsHelper

val os = { val os = sys.props("os.name"); if (os.startsWith("Windows")) "win" else if (os.startsWith("Linux")) "linux" else error("Unrecognized OS: "+os) }
val bits = sys.props("sun.arch.data.model")

val targetOs = os
val targetBits = bits

name := "proof-editor"
version := "0.1"
maintainer := "Dominique Unruh"
organization := "de.unruh"

//javaHome := Some(file(sys.props("java.home")).getAbsoluteFile)

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
// https://mvnrepository.com/artifact/org.antlr/antlr-runtime
libraryDependencies += "org.antlr" % "antlr-runtime" % "3.0.1" // Needed for openmath-1.4.0.jar
//libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
//libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.2"

libraryDependencies += "com.thoughtworks.each" %% "each" % "2.0.0"
//libraryDependencies += "org.pelotom" %% "effectful" % "1.0.1"


scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"

resourceDirectory in Compile := baseDirectory.value / "src"
managedResourceDirectories in Compile += baseDirectory.value / "resources"

excludeFilter in unmanagedResources <<= (excludeFilter in unmanagedResources) { _ || "*.java" || "*.scala" || "*~" || "*.orig" }

sourcesInBase := false
scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

enableDownloads

antlr4Settings
antlr4PackageName in Antlr4 := Some("cmathml")
sourceDirectory in Antlr4 := baseDirectory.value / "src"
compileOrder in Compile := CompileOrder.Mixed
javaSource in Antlr4 := (sourceManaged in Compile).value

resourceGenerators in Compile += installResources.taskValue

dependencyClasspath in Compile ++= installJars.value
dependencyClasspath in Test ++= installJars.value

envVars := Map("LD_LIBRARY_PATH" -> (baseDirectory.value / "lib" / (os+bits)).getAbsolutePath)

fork in Test := true // Needed because otherwise we try to load the same shared lib in different classloaders during consecutive test runs

JFX.mainClass := Some("testapp.Launch")

jfxSettings


z3Version := "4.4.1"

cancelable in Global := true

enablePlugins(JavaAppPackaging)
enablePlugins(JDKPackagerPlugin)

stagingDirectory in Universal := (target in Universal).value / s"stage_$targetOs$targetBits"
NativePackagerKeys.bashScriptExtraDefines += "export LD_LIBRARY_PATH=$lib_dir"
NativePackagerKeys.batScriptExtraDefines += """cd %APP_LIB_DIR%"""

//def dirToLibMapping(dir:File) = (dir.*** --- dir) pair {x => relativeTo(dir)(x).map("lib/"+_)}

mappings in Universal ++= { targetOs+targetBits match {
  case "linux64" =>
    installZ3Linux64.value
    MappingsHelper.contentOf(baseDirectory.value/"lib"/"linux64")
  case "win32" =>
    installZ3Win32.value
    MappingsHelper.contentOf(baseDirectory.value/"lib"/"win32")
  case "win64" =>
    installZ3Win64.value
    MappingsHelper.contentOf(baseDirectory.value/"lib"/"win64")
  case t => sys.error(s"Don't know how to handle target $t")
}}

jdkPackagerType := "image"

packageBin in JDKPackager := {
  assert(targetOs==os)
  // TODO: same needed for #bits?
  (packageBin in JDKPackager).value
}

antPackagerTasks in JDKPackager := Some((file(sys.props("java.home")) / "../lib/ant-javafx.jar").getCanonicalFile)


val dropboxDirectory = file("E:\\Dropbox\\share\\merily-salura\\proof-editor")
lazy val uploadToDropbox = taskKey[Unit]("Upload Windows binaries to my dropbox")
uploadToDropbox := {
  assert(os == "win")
  (packageBin in JDKPackager).value
  val dir = (target in JDKPackager).value / "bundles" / (name in JDKPackager).value
  assert(dir.isDirectory)
  IO.delete(dropboxDirectory)
  IO.copyDirectory(dir,dropboxDirectory)
}
