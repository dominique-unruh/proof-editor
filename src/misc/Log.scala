package misc

import java.io.File

import scala.xml.Node

object Log {
  private val pfxlen = 19
  private def mkPfx(line:sourcecode.Line, file:sourcecode.File) : String = {
    val filebase = new File(file.value).getName.stripSuffix(".scala")
    val linestr = line.value.toString
    val len = pfxlen - 1 - filebase.length - linestr.length
    val pad = if (len>0) " " * len else ""
    s"$filebase:$linestr$pad"
  }
  def debug(msg:String, args:Any*)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    val argsStr = if (args.isEmpty) "" else " ("+args.mkString(", ")+")"
    println(s"${mkPfx(line,file)} DEBUG $msg$argsStr")
  }
  def info(msg:String, args:Any*)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    val filebase = new File(file.value).getName.stripSuffix(".scala")
    val argsStr = if (args.isEmpty) "" else " ("+args.mkString(", ")+")"
    println(s"${mkPfx(line,file)} INFO  $msg$argsStr")
  }
  def warn(msg:String, args:Any*)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    val filebase = new File(file.value).getName.stripSuffix(".scala")
    val argsStr = if (args.isEmpty) "" else " ("+args.mkString(", ")+")"
    Console.err.println(s"${mkPfx(line,file)} WARN  $msg$argsStr")
  }
  def stackTrace(msg:String, e:Throwable, args:Any*)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    val filebase = new File(file.value).getName.stripSuffix(".scala")
    val argsStr = if (args.isEmpty) "" else " ("+args.mkString(", ")+")"
    Console.err.println(s"$filebase:${line.value}: WARN  $msg$argsStr")
    e.printStackTrace()
  }
  def stackTraceDebug(msg:String, e:Throwable, args:Any*)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    val filebase = new File(file.value).getName.stripSuffix(".scala")
    val argsStr = if (args.isEmpty) "" else " ("+args.mkString(", ")+")"
    Console.err.println(s"$filebase:${line.value}: DEBUG $msg$argsStr")
    e.printStackTrace(System.out)
  }
}
