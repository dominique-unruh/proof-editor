import java.io.File

import com.typesafe.sbt.packager.MappingsHelper

import scala.xml.{Elem, UnprefixedAttribute}
import scala.xml.transform.{RewriteRule, RuleTransformer}

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

/*antBuildDefn in JDKPackager := {
  val rule = new RewriteRule {
    override def transform(n:scala.xml.Node) : Seq[scala.xml.Node] = n match {
      case n @ Elem("fx","fileset",_,_,_*) if n.attribute("type").map(_.toString)==Some("data") =>  n.asInstanceOf[Elem] % new UnprefixedAttribute("type","native",scala.xml.Null)
      case _ => n
    }
  }
  var trafo = new RuleTransformer(rule)
  trafo.apply((antBuildDefn in JDKPackager).value)
}*/

/*
antBuildDefn in JDKPackager := <project name="proof-editor" default="default" basedir="." xmlns:fx="javafx:com.sun.javafx.tools.ant">
  <target name="default">
    <property name="plugin.classpath" value="C:\\Program Files\\Java\\jdk1.8.0_102\\lib\\ant-javafx.jar:E:\\svn\\proof-editor\\src\\universal\\deploy:E:\\svn\\proof-editor\\target\\universal\\jdkpackager"/>
    <taskdef resource="com/sun/javafx/tools/ant/antlib.xml" uri="javafx:com.sun.javafx.tools.ant" classpath="${plugin.classpath}"/>
    <fx:platform id="platform" javafx="8+" j2se="8+">
      <fx:jvmarg value="-Xmx768m"/>
    </fx:platform>
    <fx:application id="app" name="proof-editor" version="0.1" mainClass="testapp.TestApp" toolkit="fx">
    </fx:application>
    <fx:fileset id="jar.files" dir="E:\\svn\\proof-editor\\target\\universal\\stage_win32" type="jar">
      <include name="lib/de.unruh.proof-editor-0.1.jar"/><include name="lib/com.microsoft.z3.jar"/><include name="lib/org.scala-lang.scala-library-2.11.8.jar"/><include name="lib/org.scala-lang.modules.scala-xml_2.11-1.0.5.jar"/><include name="lib/org.scala-lang.scala-reflect-2.11.8.jar"/><include name="lib/org.scalafx.scalafx_2.11-8.0.92-R10.jar"/><include name="lib/de.unruh.proof-editor-0.1-launcher.jar"/>
    </fx:fileset>
    <fx:fileset id="data.files" dir="E:\\svn\\proof-editor\\target\\universal\\stage_win32" type="data">
      <include name="bin/proof-editor"/><include name="bin/proof-editor.bat"/><include name="lib/vcomp110.dll"/><include name="lib/libz3.dll"/><include name="lib/libz3java.dll"/>
    </fx:fileset>
    <fx:deploy outdir="E:\\svn\\proof-editor\\target\\universal\\jdkpackager" outfile="proof-editor-pkg" nativeBundles="all" verbose="true">
      <fx:preferences install="true" menu="true" shortcut="true"/>
      <fx:application refid="app"/>
      <fx:platform refid="platform"/>
      <fx:info id="info" title="proof-editor" description="proof-editor" vendor="Dominique Unruh">
      </fx:info>
      <fx:resources>
        <fx:fileset refid="jar.files"/>
        <fx:fileset refid="data.files"/>
        <fx:fileset type="native" dir="../../../lib/win32" includes="libz3java.dll"/>
        <fx:fileset type="native" dir="../../../lib/win32" includes="libz3.dll"/>
        <fx:fileset type="native" dir="../../../lib/win32" includes="vcomp110.dll"/>
      </fx:resources>
      <fx:bundleArgument arg="mainJar" value="lib/de.unruh.proof-editor-0.1-launcher.jar"/>
    </fx:deploy>
  </target>
</project>
*/

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
