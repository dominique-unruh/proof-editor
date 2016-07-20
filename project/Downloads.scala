import java.io.File
import java.net.URL

import sbt.{IO, Process, _}

import scala.collection.mutable.ArrayBuffer

object Downloads extends AutoPlugin {
  object autoImport {
    lazy val installResources = taskKey[Seq[File]]("Download and build dependencies - if needed; returns list of resource files")
    lazy val installJars = taskKey[Seq[File]]("Download and build dependencies - if needed; returns list of jar files")
    lazy val downloadMathQuill = taskKey[Unit]("Download and build MathQuill")
    lazy val downloadJQuery = taskKey[Unit]("Download JQuery")
    lazy val downloadZ3 = taskKey[Unit]("Download Z3")
    lazy val enableDownloads = settings
  }

  def settings = Seq(
      autoImport.downloadMathQuill := downloadMathQuill(Keys.baseDirectory.value),
      autoImport.downloadJQuery := downloadJQuery(Keys.baseDirectory.value),
      autoImport.downloadZ3 := downloadZ3(Keys.baseDirectory.value),
      autoImport.installResources := installResources(Keys.baseDirectory.value),
      autoImport.installJars := installJars(Keys.baseDirectory.value)
    )

//  val base = Keys.baseDirectory.value

  // ---- MathQuill ----
  val mathquillTargetDir = "resources/ui/mathview/mathquill"
  private def downloadMathQuill(base:File) = {
    println("Downloading MathQuill")
    val extrdir = base / "tmp/mathquill-extract"
    IO.delete(extrdir)
    IO.delete(base/mathquillTargetDir)
    IO.createDirectory(extrdir)
    IO.download(new URL("https://github.com/dominique-unruh/mathquill/archive/for-proof-editor.zip"), base / "tmp/mathquill.zip")
    if (Process("unzip ../mathquill.zip", extrdir).! != 0) sys.error("unzip failed")
    val dir = extrdir.listFiles()(0)
    if (Process("make", dir).! != 0) sys.error("make failed")
    IO.move(dir/"build", base/mathquillTargetDir)
  }

  // ---- Z3 ----
  val z3TargetDir = "target/z3"
  private def downloadZ3(version:String,os:String,base:File) : File = {
    println(s"Downloading Z3 - $os")
    val dir = base / s"tmp/z3-$os"
    IO.delete(dir)
    val files = IO.unzipURL(new URL(s"https://github.com/Z3Prover/z3/releases/download/z3-$version/z3-$version-$os.zip"), dir)
    val actualDir = dir / s"z3-$version-$os"
    actualDir
  }

  private def downloadZ3(base:File) : Unit = {
    val target = base / z3TargetDir
    IO.createDirectory(target)
    var version = "4.4.1"
    val ubuntudir = downloadZ3(version,"x64-ubuntu-14.04",base)
    println("Ubuntu dir: "+ubuntudir)
    def move(z3dir:File,file:String,target:File) =
      IO.move(z3dir / file, target / (z3dir/file).name)
    move(ubuntudir, "bin/com.microsoft.z3.jar", target)
    move(ubuntudir, "bin/libz3.so", target/"ubuntu64")
    move(ubuntudir, "bin/libz3java.so", target/"ubuntu64")
    //val windir = downloadZ3(version,"x64-win",base)
    //move(windir, "bin/libz3.dll", target/"win64")
    //move(windir, "bin/libz3java.dll", target/"win64")
    //move(windir, "bin/vcomp110.dll", target/"win64")
    val windir32 = downloadZ3(version,"x86-win",base)
    move(windir32, "bin/libz3.dll", target/"win32")
    move(windir32, "bin/libz3java.dll", target/"win32")
    move(windir32, "bin/vcomp110.dll", target/"win32")
  }

  scala.reflect.runtime.universe.typeOf[String]

  // ---- JQuery ----
  val jqueryFile = "resources/ui/mathview/jquery.js"
  def downloadJQuery(base:File) = {
    println("Downloading JQuery")
    IO.download(new URL("https://code.jquery.com/jquery-2.2.4.js"), base/jqueryFile)
  }

  def recursiveFiles(base:File, roots: String*) : Seq[File] = {
    val absroots = roots map {base/_}
    val descendents = absroots flatMap { Path.allSubpaths(_) } map {_._1}
    absroots ++ descendents
  }



  def installDependencies(base:File) = {
    if (!(base/jqueryFile).exists()) downloadJQuery(base)
    if (!(base/mathquillTargetDir).isDirectory()) downloadMathQuill(base)
    if (!(base/z3TargetDir).isDirectory) downloadZ3(base)
  }

  def installJars(base:File) = {
    installDependencies(base)
    (base/z3TargetDir) ** "*.jar" get
  }

  def installResources(base:File) = {
    installDependencies(base)
    recursiveFiles(base, jqueryFile, mathquillTargetDir)
  }
}