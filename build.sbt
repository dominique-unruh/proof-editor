name := "my-javafx-application"

scalaVersion := "2.11.8"

scalaSource in Compile := baseDirectory.value / "src"
antlr4Settings
antlr4PackageName in Antlr4 := Some("mathview")
sourceDirectory in Antlr4 := baseDirectory.value / "src"

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"

compileOrder in Compile := CompileOrder.JavaThenScala

unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/ext/jfxrt.jar"))

jfxSettings

JFX.mainClass := Some("mypackage.MyJavaFXApplication")
