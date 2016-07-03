/**
  * Created by unruh on 7/3/16.
  */
abstract class CMathML

case class CN(n:BigDecimal) extends CMathML
case class CI(v:String) extends CMathML
case class CSymbol(cd:String,name:String) extends CMathML
//case class CS(str:String) extends CMathML
case class Apply(hd:CMathML, args: CMathML*) extends CMathML

case class Path(val path : List[Int]) extends AnyVal {
  def prepend(i:Int) = new Path(i::path)
  override def toString() = path.mkString("-")
  def toPathRev() = new PathRev(path.reverse)
}
object Path {
  val empty = Path(List.empty)
  val emptyRev = PathRev(List.empty)
}
case class PathRev(val path : List[Int]) extends AnyVal {
  def append(i:Int) = new PathRev(i::path)
  def toPath() = new Path(path.reverse)
  override def toString() = toPath().toString()
}
