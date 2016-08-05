package tryouts

import java.io._
import java.net.{URL, URLClassLoader}
import javafx.scene.input.DataFormat

class ClipAttack {

}

case class X(x:Int) {
  println("Constructed")
}


object ClipAttack {
  def main(args: Array[String]): Unit = {
    val url = getClass.getResource("/")
    println(url)
    val cl = new URLClassLoader(Array[URL](url))
    val clazz = cl.loadClass("tryouts.Y")
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(cl)
    val ba = baos.toByteArray




//    clazz.getConstructor().newInstance()

    println(baos.toString)
    println(ba.map(_.toString).mkString(","))
//    val ba2 = Array[Byte](-84,-19,0,5,115,114,0,9,116,114,121,111,117,116,115,46,88,125,1,15,4,14,81,-126,73,2,0,1,73,0,1,120,120,112,0,0,0,3)
//    println(ba2.map(_.toString).mkString(","))
//    assert(ba==ba2)


    val bais = new ByteArrayInputStream(ba)
    val ois = new ObjectInputStream(bais) {
      override protected def resolveClass(desc: ObjectStreamClass): Class[_] =
        Class.forName(desc.getName, false, Thread.currentThread.getContextClassLoader)
    }

    val obj = ois.readObject()
    println(obj)
  }
}