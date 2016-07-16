import java.io.File
import java.net.URL

import sbt.{IO, Process, _}

import scala.collection.mutable.ArrayBuffer

object Downloads extends AutoPlugin {
  object autoImport {
    lazy val installDependencies = taskKey[Seq[File]]("Download and build dependencies - if needed")
    lazy val downloadMathQuill = taskKey[Unit]("Download and build MathQuill")
    lazy val downloadJQuery = taskKey[Unit]("Download JQuery")
    lazy val enableDownloads = settings
  }

  def settings = Seq(
      autoImport.downloadMathQuill := downloadMathQuill(Keys.baseDirectory.value),
      autoImport.downloadJQuery := downloadJQuery(Keys.baseDirectory.value),
      autoImport.installDependencies := installDependencies(Keys.baseDirectory.value)
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
    recursiveFiles(base, jqueryFile, mathquillTargetDir)
  }

}