name := "proof-editor"

scalaVersion := "2.11.8"
libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"

scalaSource in Compile := baseDirectory.value / "src"
resourceDirectory in Compile := baseDirectory.value / "resources"
sourcesInBase := false

antlr4Settings
antlr4PackageName in Antlr4 := Some("mathview")
sourceDirectory in Antlr4 := baseDirectory.value / "src"
compileOrder in Compile := CompileOrder.JavaThenScala


val mathquillTargetDir = file("resources/mathquill")
def buildMathQuill_ = {
  println("Downloading MathQuill")
  val extrdir = new File("tmp/mathquill-extract")
  IO.delete(extrdir)
  IO.delete(mathquillTargetDir)
  IO.createDirectory(extrdir)
  IO.download(new URL("https://github.com/dominique-unruh/mathquill/archive/for-proof-editor.zip"), file("tmp/mathquill.zip"))
  if (Process("unzip ../mathquill.zip",extrdir).! != 0) sys.error("unzip failed")
  //  IO.unzipURL(new URL("https://github.com/mathquill/mathquill/archive/v0.10.1.zip"), extrdir)
  val dir = extrdir.listFiles()(0)
  if (Process("make",dir).! != 0) sys.error("make failed")
  IO.move(new File(dir,"build"),mathquillTargetDir)
}
lazy val buildMathQuill = taskKey[Unit]("Download and build MathQuill")
buildMathQuill := { buildMathQuill_ }

// ---- JQuery ----
val jqueryFile = file("resources/jquery.js")
def downloadJQuery_ = {
  println("Downloading JQuery")
  IO.download(new URL("https://code.jquery.com/jquery-2.2.4.js"), jqueryFile)
}
lazy val downloadJQuery = taskKey[Unit]("Download JQuery")
downloadJQuery := { downloadJQuery_ }

/*// --
val resizeSensorFile = file("resources/ResizeSensor.js")
def downloadResizeSensor_ = {
  println("Downloading ResizeSensor")
  IO.download(new URL("https://raw.githubusercontent.com/marcj/css-element-queries/0.3.2/src/ResizeSensor.js"), resizeSensorFile)
}
lazy val downloadResizeSensor = taskKey[Unit]("Download ResizeSensor")
downloadResizeSensor := { downloadResizeSensor_} */

lazy val installDependencies = taskKey[Unit]("Download and build dependencies - if needed")
installDependencies := {
  if (!jqueryFile.exists()) downloadJQuery_
  if (!mathquillTargetDir.isDirectory()) buildMathQuill_
//  if (!resizeSensorFile.exists()) downloadResizeSensor_
}


compile <<= (compile in Compile) dependsOn installDependencies


unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/ext/jfxrt.jar"))

mainClass in Compile := Some("misc.TestApp")
