import javafx.scene.input.Clipboard

import scala.collection.JavaConversions._

import ui.TestFxApp

object Temp {
  def main(args: Array[String]) = {
    TestFxApp.run {
      val clip = Clipboard.getSystemClipboard
      val types = clip.getContentTypes
//      println(types)
      for (t <- types)
        println(s"$t: ${clip.getContent(t)}")
      sys.exit()
    }
  }
}