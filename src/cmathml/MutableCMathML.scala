package cmathml

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed abstract class MutableCMathMLParent {
  val changeListeners = new ListBuffer[() => Unit]
  protected def fireChange() : Unit =
    for (l <- changeListeners) l()
  def addChangeListener(listener: () => Unit) =
    changeListeners += listener
  def getDocument : Option[MutableCMathMLDocument]
}

final case class MutableCMathMLDocument private () extends MutableCMathMLParent {
  private var _root : MutableCMathML = null
  def toCMathML = _root.toCMathML

  def setRoot(root: MutableCMathML) = {
    assert(!root.isAttached)
    _removeRoot()
    root.setParent(this)
    _root = root
    fireChange()
  }

  def setRoot(root : CMathML) : Unit =
    setRoot(MutableCMathML.fromCMathML(root))

  def this(root:MutableCMathML) = {
    this()
    setRoot(root)
  }

  def this(root:CMathML) = {
    this()
    setRoot(root)
  }

  def root = _root
  private def _removeRoot() : MutableCMathML = {
    val h = _root
    if (h!=null) {
      _root = null
      h.detach()
      h
    } else null
  }
  def removeHead() : MutableCMathML = {
    val hd = _removeRoot()
    if (hd!=null) fireChange()
    hd
  }

  def getDocument = Some(this)
}

sealed abstract class MutableCMathML extends MutableCMathMLParent {
  private[cmathml] def setParent(parent: MutableCMathMLParent) = {
    assert(_parent==null)
    assert(parent!=null)
    _parent = parent
    for (l <- parentListeners) l(null)
  }

  override def toString = s"[M(${Integer.toHexString(hashCode)}): ${toCMathML}]"

  private var _parent : MutableCMathMLParent = null
  def isAttached = _parent!=null
  def parent = _parent
  private[cmathml] def detach() = {
    val prev = _parent
    assert(prev!=null)
    _parent = null
    for (l <- parentListeners) l(prev)
  }
  def parentListeners = new ListBuffer[MutableCMathMLParent => Unit]
  def addParentListener(listener : MutableCMathMLParent => Unit) =
    parentListeners += listener

  def toCMathML : CMathML

  def getDocument = if (_parent==null) None else _parent.getDocument
}

object MutableCMathML {
  def fromCMathML(math:CMathML) : MutableCMathML = math match {
    case Apply(hd, args @ _*) => new MApply(fromCMathML(hd),args.map(fromCMathML(_)) : _*)
    case CI(v) => new MCI(v)
    case CN(n) => new MCN(n)
    case CSymbol(cd, name) => new MCSymbol(cd,name)
    case CError(cd, name, args @ _*) => new MCError(cd,name,args)
    case CNone() => new MCNone()
  }
}

object MApply {
  def unapplySeq(arg: MApply): Some[(MutableCMathML,Seq[MutableCMathML])] = Some((arg._head,arg._args))
}
final class MApply extends MutableCMathML {
  def setHead(head: MutableCMathML) = {
    assert(!head.isAttached)
    _removeHead()
    head.setParent(this)
    _head = head
    fireChange()
  }

  def setArgs(args: MutableCMathML*) = {
    for (a<-args) assert(!a.isAttached)
    _removeArgs()
    _args.insertAll(0,args)
    for (a <- _args)
      a.setParent(this)
    fireChange()
  }

  def this(head:MutableCMathML, args:MutableCMathML*) = {
    this()
    setHead(head)
    setArgs(args : _*)
  }
  private var _head : MutableCMathML = null
  private val _args : mutable.ArrayBuffer[MutableCMathML] = new mutable.ArrayBuffer()
  def head = _head
  def arg(i:Int) = _args(i)
  def argNum = _args.length
  private def _removeHead() : MutableCMathML = {
    val h = _head
    if (h!=null) {
      _head = null
      h.detach()
      h
    } else null
  }
  def removeHead() : MutableCMathML = {
    val hd = _removeHead()
    if (hd!=null) fireChange()
    hd
  }
  def removeArg(i:Int) : MutableCMathML = {
    val a = _removeArg(i)
    if (a!=null) fireChange()
    a
  }
  private def _removeArg(i:Int) : MutableCMathML = {
    if (i<0 || i>=_args.length) throw new IllegalArgumentException
    val a = _args(i)
    if (a!=null) {
      _args.update(i, null)
      fireChange()
      a
    } else null
  }
  private def _removeArgs() : Unit = {
    for (a <- _args)
      a.detach()
    _args.clear()
  }

  override def toCMathML: CMathML =
    Apply(_head.toCMathML, _args.map(_.toCMathML) : _*)
}
final case class MCI(private var _name:String) extends MutableCMathML {
  override def toCMathML: CMathML = CI(_name)
  def name = _name
  def name_=(name:String) = {
    _name = name
    fireChange()
  }
}
final case class MCN(val n:BigDecimal) extends MutableCMathML {
  override def toCMathML: CMathML = CN(n)
}
final case class MCSymbol(private var _cd:String, private var _name:String) extends MutableCMathML {
  override def toCMathML: CMathML = CSymbol(_cd,_name)
  def cd = _cd
  def cd_=(cd:String) = {
    _cd = cd
    fireChange()
  }
  def name = _name
  def name_=(name:String) = {
    _name = name
    fireChange()
  }
}
final case class MCError(val cd: String, val name: String, val args: Any*) extends MutableCMathML {
  override def toCMathML: CMathML = CError(cd,name,args)
}
final case class MCNone() extends MutableCMathML {
  override def toCMathML: CMathML = CNone()
}