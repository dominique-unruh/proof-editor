import java.io.File
import java.net.URL

import sbt.{IO, Process, _}

import scala.collection.mutable.ArrayBuffer
import Keys._

object Downloads extends AutoPlugin {
//  lazy val installDependencies = taskKey[Unit]("Download and build dependencies - if needed")
  lazy val installMathQuill = TaskKey[File]("Download and build MathQuill")
  lazy val installJQuery = TaskKey[File]("Download JQuery")

  def copyInto(from:File,to:File) = IO.copyFile(from, to / from.name)

  object autoImport {
    lazy val installResources = TaskKey[Seq[File]]("Download and build dependencies - if needed; returns list of resource files")
    lazy val installJars = TaskKey[Seq[File]]("Download and build dependencies - if needed; returns list of jar files")
    lazy val z3Version = SettingKey[String]("Z3 version")
    lazy val installZ3Linux64 = TaskKey[Unit]("Download and build Z3 (Linux 64)")
    lazy val installZ3Win64 = TaskKey[Unit]("Download and build Z3 (Windows 64)")
    lazy val installZ3Win32 = TaskKey[Unit]("Download and build Z3 (Windows 32)")

    lazy val enableDownloads = Seq(
      installResources := recursiveFiles(installJQuery.value,installMathQuill.value),

      installJars := { installZ3Linux64.value; ((base.value/"lib") ** "*.jar").get },

      installMathQuill := downloadMathQuill(base.value),

      installJQuery := downloadJQuery(base.value),

      installZ3Linux64 := {
        if (!(base.value/"lib/linux64/libz3java.so").exists) {
          val dir = downloadZ3(z3Version.value, "x64-ubuntu-14.04", base.value)
          copyInto(dir / "bin/com.microsoft.z3.jar", base.value / "lib")
          copyInto(dir / "bin/libz3.so", base.value / "lib/linux64")
          copyInto(dir / "bin/libz3java.so", base.value / "lib/linux64")
        }},

      installZ3Win64 := {
        if (!(base.value/"lib/win64/libz3java.dll").exists) {
          val dir = downloadZ3(z3Version.value,"x64-win",base.value)
          copyInto(dir/"bin/libz3.dll", base.value/"lib/win64")
          copyInto(dir/"bin/libz3java.dll", base.value/"lib/win64")
          copyInto(dir/"bin/vcomp110.dll", base.value/"lib/win64")
          copyInto(dir/"bin/msvcr110.dll", base.value/"lib/win64")
          copyInto(dir/"bin/msvcp110.dll", base.value/"lib/win64")
        }},

      installZ3Win32 := {
        if (!(base.value/"lib/win32/libz3java.dll").exists) {
          val dir = downloadZ3(z3Version.value,"x86-win",base.value)
          copyInto(dir/"bin/libz3.dll", base.value/"lib/win32")
          copyInto(dir/"bin/libz3java.dll", base.value/"lib/win32")
          copyInto(dir/"bin/vcomp110.dll", base.value/"lib/win32")
        }}
    )
  }

  val base = baseDirectory

  // ---- MathQuill ----
  val mathquillTargetDir = "resources/ui/mathview/mathquill"
  private def downloadMathQuill(base:File) = {
    if (!(base / mathquillTargetDir).exists) {
      println("Downloading MathQuill")
      val extrdir = base / "tmp/mathquill-extract"
      IO.delete(extrdir)
      IO.delete(base / mathquillTargetDir)
      IO.createDirectory(extrdir)
      IO.download(new URL("https://github.com/dominique-unruh/mathquill/archive/for-proof-editor.zip"), base / "tmp/mathquill.zip")
      if (Process("unzip ../mathquill.zip", extrdir).! != 0) sys.error("unzip failed")
      val dir = extrdir.listFiles()(0)
      if (Process("make", dir).! != 0) sys.error("make failed")
      IO.move(dir / "build", base / mathquillTargetDir)
    }
    base / mathquillTargetDir
  }

  // ---- Z3 ----
//  val z3TargetDir = "target/z3"
  private def downloadZ3(version:String,os:String,base:File) : File = {
    println(s"Downloading Z3 - $os")
    val dir = base / s"tmp/z3-$os"
    IO.delete(dir)
    val files = IO.unzipURL(new URL(s"https://github.com/Z3Prover/z3/releases/download/z3-$version/z3-$version-$os.zip"), dir)
    val actualDir = dir / s"z3-$version-$os"
    actualDir
  }

//  private def downloadZ3(base:File) : Unit = {
//    val target = base / "lib"
//    IO.createDirectory(target)
//    var version = "4.4.1"
//    def move(z3dir:File,file:String,target:File) =
//      IO.move(z3dir / file, target / (z3dir/file).name)
//
//    val ubuntudir = downloadZ3(version,"x64-ubuntu-14.04",base)
//    move(ubuntudir, "bin/com.microsoft.z3.jar", target)
//    move(ubuntudir, "bin/libz3.so", target/"linux-x64")
//    move(ubuntudir, "bin/libz3java.so", target/"linux-x64")
//
//    val windir = downloadZ3(version,"x64-win",base)
//    move(windir, "bin/libz3.dll", target/"windows-x64")
//    move(windir, "bin/libz3java.dll", target/"windows-x64")
//    move(windir, "bin/vcomp110.dll", target/"windows-x64")
//
//    val windir32 = downloadZ3(version,"x86-win",base)
//    move(windir32, "bin/libz3.dll", target/"windows-x86")
//    move(windir32, "bin/libz3java.dll", target/"windows-x86")
//    move(windir32, "bin/vcomp110.dll", target/"windows-x86")
//  }

  // ---- JQuery ----
  val jqueryFile = "resources/ui/mathview/jquery.js"
  def downloadJQuery(base:File) = {
    if (!(base / jqueryFile).exists) {
      println("Downloading JQuery")
      IO.download(new URL("https://code.jquery.com/jquery-2.2.4.js"), base / jqueryFile)
    }
    base / jqueryFile
  }

  def recursiveFiles(roots: File*) : Seq[File] = {
    val absroots = roots // map {base/_}
    val descendents = absroots flatMap { Path.allSubpaths(_) } map {_._1}
    absroots ++ descendents
  }



//  def installDependencies(base:File) = {
//    if (!(base/jqueryFile).exists()) downloadJQuery(base)
//    if (!(base/mathquillTargetDir).isDirectory()) downloadMathQuill(base)
//    if (!(base/"lib/com.microsoft.z3.jar").exists) downloadZ3(base)
//  }
//
//  def installJars(base:File) = {
//    installDependencies(base)
//    (base/"lib") ** "*.jar" get
//  }
//
//  def installResources(base:File) = {
//    installDependencies(base)
//    recursiveFiles(base, jqueryFile, mathquillTargetDir)
//  }
}