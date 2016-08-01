package cmathml

import cmathml.MutableCMathML.{Attributes, AttributesRO, NoAttr, fromCMathML}
import ui.mathview.{MathNode, MathViewFXExample}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer



sealed abstract class MutableCMathMLParent {
  def replace(a: MutableCMathML, b: MutableCMathML): Unit

  val changeListeners = new ListBuffer[() => Unit]
  def fireChange() : Unit =
    for (l <- changeListeners) l()
  def addChangeListener(listener: () => Unit) =
    changeListeners += listener
  def getDocument : Option[MutableCMathMLDocument]
}

final class MutableCMathMLDocument private () extends MutableCMathMLParent {
  def isEmpty: Boolean = _root match {
    case MCNone() => true
    case _ => false
  }

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
    setRoot(fromCMathML(root))

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
  def removeRoot() : MutableCMathML = {
    val hd = _removeRoot()
    if (hd!=null) fireChange()
    hd
  }

  def getDocument = Some(this)

  override def replace(a: MutableCMathML, b: MutableCMathML): Unit = {
    assert(_root==a)
    setRoot(b)
  }
}

sealed abstract class MutableCMathML(attribs : AttributesRO) extends MutableCMathMLParent {
  var node : MathNode = null
  var embeddedIn : MathNode = null

  /** Returns true is `this` is equal to or a descendant of `ancestor`
    */
  @tailrec final def isDescendantOf(ancestor: MutableCMathMLParent) : Boolean =
    if (this eq ancestor)
      true
    else if (_parent != null && _parent.isInstanceOf[MutableCMathML])
      _parent.asInstanceOf[MutableCMathML].isDescendantOf(ancestor)
    else
      false

  def copy() : MutableCMathML

  def replaceWith(m: MutableCMathML): Unit =
    _parent.replace(this,m)

  def replaceInAttributes(a: MutableCMathML, b: MutableCMathML): Unit = {
    assert(a!=null)
    assert(b!=null)
    ???
  }

  val _attributes : Attributes = new mutable.HashMap
  for ((k,a) <- _attributes)
    a match {
      case a$ : MutableCMathML =>
        assert(!a$.isAttached)
        a$.setParent(this)
        _attributes.update(k,a$)
      case a$ : CMathML =>
        val a$$ = fromCMathML(a$)
        a$$.setParent(this)
        _attributes.update(k,a$$)
      case _ =>
        _attributes.update(k,a)
    }
//  val _attributes = attributes
  val attributes : Attributes = new mutable.AbstractMap[(String,String),Any] {
    override def +=(kv: ((String, String), Any)): this.type = ??? // TODO: parent, accept CMathML
    override def -=(key: (String, String)): this.type = ??? // TODO: parent
    override def get(key: (String, String)): Option[Any] =
      _attributes.get(key)
    override def iterator: Iterator[((String, String), Any)] =
      _attributes.iterator
  }

  def setParent(parent: MutableCMathMLParent) = {
    assert(_parent==null)
    assert(parent!=null)
    _parent = parent
    for (l <- parentListeners) l(null)
  }

  override def toString = s"[M(${Integer.toHexString(System.identityHashCode(this))}): ${toCMathML}]"

  private var _parent : MutableCMathMLParent = null
  def isAttached = _parent!=null
  def parent = _parent
  private[cmathml] def detach() = {
    val prev = _parent
    assert(prev!=null)
    _parent = null
    for (l <- parentListeners) l(prev)
  }
  val parentListeners = new ListBuffer[MutableCMathMLParent => Unit]
  def addParentListener(listener : MutableCMathMLParent => Unit) =
    parentListeners += listener

  def toCMathML : CMathML

  def attributesToCMathML : CMathML.Attributes =
    attributes.mapValues({
      case m : MutableCMathML => m.toCMathML
      case x => x
    }).toMap

  def getDocument = if (_parent==null) None else _parent.getDocument
}

object MutableCMathML {
  type Attributes = mutable.Map[(String,String),Any]
  type AttributesRO = Map[(String,String),Any]
  val NoAttr : AttributesRO = Map.empty
//  private def fromCMathMLAttr(attributes:CMathML.Attributes) : AttributesRO =
//    attributes.mapValues { case m : CMathML => fromCMathML(m); case x => x }
  def fromCMathML(math:CMathML) : MutableCMathML = math match {
    case Apply(att, hd, args @ _*) => new MApply(att,fromCMathML(hd),args.map(fromCMathML(_)) : _*)
    case CI(att, v) => new MCI(att,v)
    case CN(att, n) => new MCN(att,n)
    case CSymbol(att, cd, name) => new MCSymbol(att,cd,name)
    case CError(att, cd, name, args @ _*) => new MCError(att,cd,name,args)
    case CNone(att) => new MCNone(att)
  }
}

object MApply {
  def unapplySeq(arg: MApply): Some[(MutableCMathML,Seq[MutableCMathML])] = Some((arg._head,arg._args))
}

final class MApply(attributes:AttributesRO) extends MutableCMathML(attributes) {
  def setHead(head: MutableCMathML) = {
    assert(head!=null)
    assert(!head.isAttached)
    _removeHead()
    _head = head
    head.setParent(this)
    fireChange()
  }
  def setArg(i:Int, m:MutableCMathML) = {
    assert(!m.isAttached)
    _removeArg(i)
    _args.update(i,m)
    m.setParent(this)
    fireChange()
  }

  def setArgs(args: MutableCMathML*) = {
    for (a<-args) assert(!a.isAttached)
    _removeArgs()
    _args.insertAll(0,args)
    for (a <- _args) {
      assert(!a.isAttached) // Could be violated if _args contains duplicates
      a.setParent(this)
    }
    fireChange()
  }

  def this(attributes: AttributesRO, head:MutableCMathML, args:MutableCMathML*) = {
    this(attributes)
    setHead(head)
    setArgs(args : _*)
  }
  def this(head:MutableCMathML, args:MutableCMathML*) = {
    this(NoAttr)
    setHead(head)
    setArgs(args : _*)
  }
  def this(head:CMathML, args:MutableCMathML*) = {
    this(NoAttr)
    setHead(fromCMathML(head))
    setArgs(args : _*)
  }


  private var _head : MutableCMathML = null
  val _args : mutable.ArrayBuffer[MutableCMathML] = new mutable.ArrayBuffer()
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
    _head = new MCNone()
    if (hd!=null) fireChange()
    hd
  }
  def removeArg(i:Int) : MutableCMathML = {
    val a = _removeArg(i)
    _args(i) = new MCNone()
    if (a!=null) fireChange()
    a
  }
  def _removeArg(i:Int) : MutableCMathML = {
    if (i<0 || i>=_args.length) throw new IllegalArgumentException
    val a = _args(i)
    if (a!=null) {
      _args.update(i, null)
      a.detach()
      a
    } else null
  }
  private def _removeArgs() : Unit = {
    for (a <- _args)
      a.detach()
    _args.clear()
  }

  override def toCMathML: CMathML =
    Apply(attributesToCMathML, _head.toCMathML, _args.map(_.toCMathML) : _*)

  override def replace(a: MutableCMathML, b: MutableCMathML): Unit = {
    if (_head==a) setHead(b)
    else {
      val i = _args.indexOf(a)
      if (i>=0)
        setArg(i,b)
      else
        replaceInAttributes(a,b)
    }
  }

  override def copy(): MutableCMathML = ???
}
final class MCI(attributes: AttributesRO, private var _name:String) extends MutableCMathML(attributes) {
  def this(name:String) = this(NoAttr,name)

  override def toCMathML: CMathML = CI(attributesToCMathML,_name)
  def name = _name
  def name_=(name:String) = {
    _name = name
    fireChange()
  }

  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MCI = {
    assert(attributes == NoAttr) // TODO: copy() even when attributes exist (must copy all MutableCMathML attributes)
    new MCI(NoAttr, _name)
  }
}
final class MCN(attributes: AttributesRO, val n:BigDecimal) extends MutableCMathML(attributes) {
  override def toCMathML: CMathML = CN(attributesToCMathML,n)

  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MutableCMathML = ???
}
object MCN {
  def unapply(that:MCN) = Some(that.n)
}
object MCSymbol {
  def unapply(that:MCSymbol) = Some((that._cd,that._name))
}
final class MCSymbol(attributes: AttributesRO, private var _cd:String, private var _name:String) extends MutableCMathML(attributes) {
  def this(m:CSymbol) = this(m.attributes,m.cd,m.name)
  override def toCMathML: CMathML = CSymbol(attributesToCMathML,_cd,_name)
  def cd = _cd
  def cd_=(cd:String) = {
    assert(cd!=null)
    _cd = cd
    fireChange()
  }
  def name = _name
  def name_=(name:String) = {
    assert(name!=null)
    _name = name
    fireChange()
  }
  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MCSymbol = {
    assert(attributes == NoAttr) // TODO: copy() even when attributes exist (must copy all MutableCMathML attributes)
    new MCSymbol(NoAttr, _cd, _name)
  }
}
final class MCError(attributes: AttributesRO, val cd: String, val name: String, val args: Any*) extends MutableCMathML(attributes) {
  override def toCMathML: CMathML = CError(attributesToCMathML,cd,name,args)
  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MutableCMathML = ???
}
object MCNone {
  def unapply(that:MCNone) = true
}
final class MCNone(attributes: AttributesRO = NoAttr) extends MutableCMathML(attributes) {
  override def toCMathML: CMathML = CNone(attributesToCMathML)
  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MutableCMathML = ???
}