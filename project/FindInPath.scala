import java.io.{File, FileNotFoundException}

import sbt.AutoPlugin
import sbt.RichFile

object FindInPath extends AutoPlugin {

  object autoImport {
    def findFile(baseNames : Seq[String], path : File*) : File = {
      for (p <- path)
        for (f <- baseNames) {
          val pf = new File(p,f)
          if (pf.exists) return pf.getAbsoluteFile
        }
      throw new FileNotFoundException(baseNames.mkString(" or ")+"  in  "+path.mkString(" or "))
    }

    def findFile(baseName : String, path : File*) : File = findFile(List(baseName), path : _*)
    def findFile(baseName : String, path : String) : File = findFile(List(baseName), splitPath(path) : _*)
  }

  def splitPath(path : String) : Seq[File] = for { f <- path.split(File.pathSeparator) } yield new File(f)
}
