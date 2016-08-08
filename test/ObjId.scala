import java.lang.management.ManagementFactory
import java.util.regex.Pattern

import com.sun.jdi.Bootstrap
import com.sun.jdi.connect.LaunchingConnector
import com.sun.tools.attach.VirtualMachine
import com.sun.tools.jdi.ProcessAttachingConnector

import scala.collection.JavaConversions._
import scala.util.matching.Regex
/**
  * Created by unruh on 8/7/16.
  */
class ObjId(val name:String) {
}

object ObjId {
  def mapFind[A,B](l:Seq[A],f:A=>Option[B]) : Option[B] = {
    for (x <- l)
      f(x) match {
        case None =>
        case Some(y) => return Some(y)
      }
    None
  }

  def guessAddress() : Option[(String,Int)] = {
    val args = ManagementFactory.getRuntimeMXBean.getInputArguments
    val pat = "-agentlib:.*address=([a-zA-Z0-9.]*):([0-9]*).*".r
    ObjId.mapFind[String,(String,Int)](args, {case `pat`(h,p) => Some((h,p.toInt)); case _ => None })
    Some("127.0.0.1",44444)
  }

  def getProcessAttach() =
    Bootstrap.virtualMachineManager().attachingConnectors().find(ac => ac.name.endsWith("ProcessAttach"))

  def getSocketAttach() =
    Bootstrap.virtualMachineManager().attachingConnectors().find(ac => ac.name.endsWith("SocketAttach"))

  def getPid =
    ManagementFactory.getRuntimeMXBean.getName.split('@')(0).toInt

  def getVMViaSocket(port : Int) = {
    val ac = getSocketAttach().get
//    val (_,port1) = guessAddress().get
    val args = ac.defaultArguments
    args("hostname").setValue("127.0.0.1")
    args("port").setValue(port.toString)
    val vm = ac.attach(args)
    vm
  }

  def main(args: Array[String]): Unit = {
    var o1 = new ObjId("bla")
    var o2 = new ObjId("blu")
//    val pid = ManagementFactory.getRuntimeMXBean.getName.split('@')(0).toInt
//    println(pid)
//    val vm = VirtualMachine.attach(pid.toString)
//    val a = vm.loadAgent("jdwp")
//    Thread.sleep(100000)


//    Thread.sleep(2000)

    val x = "hello"

    println(o1)

    o1 = o2

    val ac = getProcessAttach().get
    val pid = getPid
    val args = ac.defaultArguments
    println(args)
    args("pid").setValue(pid.toString)
    println(args)
    val vm = ac.attach(args)

//    ProcessAttachingConnector
//    val vm = getVMViaSocket(44444)
//    val vm2 = getVMViaSocket(44445)

    println(vm.allClasses().length)
//    println(vm2.allClasses().length)
//    Thread.sleep(10000)
//    for (c <- Bootstrap.virtualMachineManager().attachingConnectors())
//      println(c.name())
//    com.sun.jdi.connect.AttachingConnector



  }
}