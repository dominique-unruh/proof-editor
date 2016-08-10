package cmathml

import java.math.BigInteger

import cmathml.MutableCMathML.{Attributes, AttributesRO, MNoAttr, fromCMathML}
import misc.Log
import ui.mathview.MathViewFX.CursorSide

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


// TODO: add isNCName for cd's and name's everywhere

sealed abstract class MutableCMathMLParent {
  def replace(a: MutableCMathML, b: MutableCMathML): Unit
  private[cmathml] def extendPath(child : MutableCMathML, childPath : Path) : Path

  val changeListeners = new ListBuffer[() => Unit]
  protected def fireChange() : Unit = {
    for (doc <- getDocument) doc.fireGlobalChange(this)
    for (l <- changeListeners)
      try l()
      catch { case e: Throwable => Log.stackTrace("while firing changeListeners", e) }
  }
  def addChangeListener(listener: () => Unit) =
    changeListeners += listener
  def getDocument : Option[MutableCMathMLDocument]
}

final class MutableCMathMLDocument private () extends MutableCMathMLParent {
  def subterm(path: Path): MutableCMathML = root.subterm(path)

  def isEmpty: Boolean = _root match {
    case MCNone() => true
    case _ => false
  }

  private[cmathml] def fireGlobalChange(m: MutableCMathMLParent) =
    for (l <- globalChangeListeners)
      try l(m)
      catch {
        case e: Throwable => Log.stackTrace("while firing globalChangeListeners",e)
      }

  val globalChangeListeners = new ListBuffer[MutableCMathMLParent => Unit]
  def addGlobalChangeListener(listener: MutableCMathMLParent => Unit) =
    globalChangeListeners += listener

  private var _root : MutableCMathML = _
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

  override private[cmathml] def extendPath(child: MutableCMathML, childPath: Path): Path = childPath
}

sealed abstract class MutableCMathML(attribs : AttributesRO) extends MutableCMathMLParent {
  def getPath: Path = parent.extendPath(this, Path.empty)

  for ((cd,name) <- attribs.keys) { assert(CMathML.isNCName(cd)); assert(CMathML.isNCName(name)) }
  /** Returns true is `this` is equal to or a descendant of `ancestor`
    */
  @tailrec final def isDescendantOf(ancestor: MutableCMathMLParent) : Boolean =
    if (this eq ancestor)
      true
    else if (_parent != null && _parent.isInstanceOf[MutableCMathML])
      _parent.asInstanceOf[MutableCMathML].isDescendantOf(ancestor)
    else
      false

  /** with promise that path!=empty and path.head >= 0 */
  protected def subterm$(path: Path): MutableCMathML

  def subterm(path: Path): MutableCMathML =
    if (path.isEmpty) this
    else {
      val hd = path.head
      if (hd < 0) ??? // in attributes
      else subterm$(path)
    }


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

  private[cmathml] def setParent(parent: MutableCMathMLParent) = {
    assert(_parent==null)
    assert(parent!=null)
    _parent = parent
    for (l <- parentListeners)
      try l(null)
      catch {
        case e: Throwable => Log.stackTrace("while firing parentListeners",e)
      }
  }

  override def toString = s"[M(${Integer.toHexString(System.identityHashCode(this))}): $toCMathML]"

  private var _parent : MutableCMathMLParent = _
  def isAttached = _parent!=null
  def parent = _parent
  private[cmathml] def detach() = {
    val prev = _parent
    assert(prev!=null)
    _parent = null
    for (l <- parentListeners)
      try l(prev)
      catch {
        case e: Throwable => Log.stackTrace("while firing globalChangeListeners",e)
      }
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
  val MNoAttr : AttributesRO = Map.empty

  def fromCMathML(math:CMathML) : MutableCMathML = math match {
    case Apply(att, hd, args @ _*) => new MApply(att,fromCMathML(hd),args.map(fromCMathML) : _*)
    case CI(att, v) => new MCI(att,v)
    case CN(att, n) => new MCN(att,n)
    case CSymbol(att, cd, name) => new MCSymbol(att,cd,name)
    case CError(att, cd, name, args @ _*) => new MCError(att,cd,name,args)
    case CNone(att) => new MCNone(att)
    case Bind(att, hd, vars, arg) => new MBind(att,fromCMathML(hd),vars.map(fromCMathML(_).asInstanceOf[MCI]),fromCMathML(arg))
  }
}

final class MApply private (attributes:AttributesRO) extends MutableCMathML(attributes) {
  def args = _args.iterator

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
    this(MNoAttr)
    setHead(head)
    setArgs(args : _*)
  }
  def this(head:CMathML, args:MutableCMathML*) = {
    this(MNoAttr)
    setHead(fromCMathML(head))
    setArgs(args : _*)
  }


  private var _head : MutableCMathML = _
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
  private def _removeArg(i:Int) : MutableCMathML = {
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

  override protected def subterm$(path: Path): MutableCMathML = path.head match {
    case 0 => head.subterm(path.tail)
    case i => arg(i-1).subterm(path.tail)
  }

  override private[cmathml] def extendPath(child: MutableCMathML, childPath: Path): Path =
    if (_head eq child) childPath.prepend(0)
    else {
      val i = _args.indexOf(child)
      if (i>=0) childPath.prepend(i+1)
      else ??? // in attributes
    }
}

object MApply {
  def unapplySeq(arg: MApply): Some[(MutableCMathML,Seq[MutableCMathML])] = Some((arg._head,arg._args))

  object IsHead {
    def unapply(tuple: (MApply, MutableCMathML)): Boolean = tuple._1._head eq tuple._2
  }

  object IsArg {
    def unapply(tuple: (MApply, MutableCMathML)): Option[Int] =
      tuple._1._args.indexOf(tuple._2) match {
        case -1 => None
        case i => Some(i)
      }
  }
}

final class MBind private (attributes:AttributesRO) extends MutableCMathML(attributes) {
  private var _head : MutableCMathML = _
  private val _vars : mutable.ArrayBuffer[MCILike] = new mutable.ArrayBuffer()
  private var _body : MutableCMathML = _

  def vars = _vars.iterator

  def setHead(head: MutableCMathML) = {
    assert(head!=null)
    assert(!head.isAttached)
    _removeHead()
    _head = head
    head.setParent(this)
    fireChange()
  }
  def setBody(body: MutableCMathML) = {
    assert(body!=null)
    assert(!body.isAttached)
    _removeBody()
    _body = body
    body.setParent(this)
    fireChange()
  }
  def setVar(i:Int, m:MCILike) = {
    assert(!m.isAttached)
    _removeVar(i)
    _vars.update(i,m)
    m.setParent(this)
    fireChange()
  }

  def setVars(vars: MCILike*) = {
    for (a<-vars) assert(!a.isAttached)
    _removeVars()
    _vars.insertAll(0,vars)
    for (a <- _vars) {
      assert(!a.isAttached) // Could be violated if _args contains duplicates
      a.setParent(this)
    }
    fireChange()
  }

  def this(attributes: AttributesRO, head:MutableCMathML, vars:Seq[MCILike], body:MutableCMathML) = {
    this(attributes)
    setHead(head)
    setVars(vars:_*)
    setBody(body)
  }
  def this(head:MutableCMathML, vars:Seq[MCILike], body:MutableCMathML) =
    this(MNoAttr,head,vars,body)
  def this(head:CMathML, vars:Seq[MCILike], arg:MutableCMathML) =
    this(fromCMathML(head),vars,arg)

  def head = _head
  def variable(i:Int) = _vars(i)
  def varNum = _vars.length
  private def _removeHead() : MutableCMathML = {
    val h = _head
    if (h!=null) {
      _head = null
      h.detach()
      h
    } else null
  }
  private def _removeBody() : MutableCMathML = {
    val b = _body
    if (b!=null) {
      _body = null
      b.detach()
      b
    } else null
  }
  def removeHead() : MutableCMathML = {
    val hd = _removeHead()
    _head = new MCNone()
    if (hd!=null) fireChange()
    hd
  }
  def removeVar(i:Int) : MutableCMathML = {
    val a = _removeVar(i)
    _vars(i) = new MCNone()
    if (a!=null) fireChange()
    a
  }
  private def _removeVar(i:Int) : MutableCMathML = {
    if (i<0 || i>=_vars.length) throw new IllegalArgumentException
    val a = _vars(i)
    if (a!=null) {
      _vars.update(i, null)
      a.detach()
      a
    } else null
  }
  private def _removeVars() : Unit = {
    for (a <- _vars)
      a.detach()
    _vars.clear()
  }

  override def toCMathML: CMathML =
    Bind(attributesToCMathML, _head.toCMathML, _vars.map(_.toCMathML), _body.toCMathML)

  override def replace(a: MutableCMathML, b: MutableCMathML): Unit = {
    if (_head==a) setHead(b)
    else if (_body==a) setBody(b)
    else {
      val i = _vars.indexOf(a)
      if (i>=0)
        b match {
          case b$: MCILike => setVar(i, b$)
          case _ => throw new InvalidType("replacing variable binder by " + b.getClass)
        }
      else
        replaceInAttributes(a,b)
    }
  }

  override def copy(): MutableCMathML = ???

  override protected def subterm$(path: Path): MutableCMathML = ???

  override private[cmathml] def extendPath(child: MutableCMathML, childPath: Path): Path = ???
}
object MBind {
  def unapply(arg: MBind): Some[(MutableCMathML,Seq[MCILike],MutableCMathML)] = Some((arg._head,arg._vars,arg._body))

  object IsHead {
    def unapply(tuple: (MBind, MutableCMathML)): Boolean = tuple._1._head eq tuple._2
  }
  object IsBody {
    def unapply(tuple: (MBind, MutableCMathML)): Boolean = tuple._1._body eq tuple._2
  }

  object IsVar {
    def unapply(tuple: (MBind, MutableCMathML)): Option[Int] =
      tuple._1._vars.indexOf(tuple._2) match {
        case -1 => None
        case i => Some(i)
      }
  }

}

/** An MCI or an MCNone */
sealed trait MCILike extends MutableCMathML {
  override def toCMathML : CILike
}

final class MCI(attributes: AttributesRO, private var _name:String) extends MutableCMathML(attributes) with MCILike {
  def this(name:String) = this(MNoAttr,name)

  override def toCMathML: CI = CI(attributesToCMathML,_name)
  def name = _name
  def name_=(name:String) = {
    _name = name
    fireChange()
  }

  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MCI = {
    assert(attributes == MNoAttr) // TODO: copy() even when attributes exist (must copy all MutableCMathML attributes)
    new MCI(MNoAttr, _name)
  }

  override protected def subterm$(path: Path): MutableCMathML = ???

  override private[cmathml] def extendPath(child: MutableCMathML, childPath: Path): Path = ???
}
object MCI {
  def unapply(that:MCI) = Some(that._name)
}

final class MCN(attributes: AttributesRO, private var _n:BigDecimal) extends MutableCMathML(attributes) {
  assert(_n.mc.getPrecision==0)
  assert(_n.mc.getRoundingMode==java.math.RoundingMode.UNNECESSARY)
  override def toCMathML: CMathML = CN(attributesToCMathML,n)

  def this(d:BigDecimal) = this(MNoAttr,d)
  def this(i:BigInteger) = this(MNoAttr,BigDecimal(i,CN.MATHCONTEXT))
  def this(i:Int) = this(MNoAttr,BigDecimal(i,CN.MATHCONTEXT))
  def this(d:Double) = this(MNoAttr,BigDecimal.exact(d)(CN.MATHCONTEXT))
  def this(s:String) = this(MNoAttr,BigDecimal(s,CN.MATHCONTEXT))

  def n = _n
  def n_=(n:BigDecimal): Unit = {
    assert(n.mc.getPrecision==0)
    assert(n.mc.getRoundingMode==java.math.RoundingMode.UNNECESSARY)
    _n = n
    fireChange()
  }

  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MutableCMathML = ???

  override protected def subterm$(path: Path): MutableCMathML = ???

  override private[cmathml] def extendPath(child: MutableCMathML, childPath: Path): Path = ???
}
object MCN {
  def unapply(that:MCN) = Some(that.n)
}
object MCSymbol {
  def unapply(that:MCSymbol) = Some((that._cd,that._name))
  def apply(cd:String, name:String) = new MCSymbol(MNoAttr,cd,name)
}
final class MCSymbol(attributes: AttributesRO, private var _cd:String, private var _name:String) extends MutableCMathML(attributes) {
  assert(CMathML.isNCName(_cd))
  assert(CMathML.isNCName(_name))
  def this(m:CSymbol) = this(m.attributes,m.cd,m.name)
  override def toCMathML: CMathML = CSymbol(attributesToCMathML,_cd,_name)
  def cd = _cd
  def cd_=(cd:String) = {
    assert(CMathML.isNCName(cd))
    _cd = cd
    fireChange()
  }
  def name = _name
  def name_=(name:String) = {
    assert(CMathML.isNCName(name))
    assert(name!=null)
    _name = name
    fireChange()
  }
  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MCSymbol = {
    assert(attributes == MNoAttr) // TODO: copy() even when attributes exist (must copy all MutableCMathML attributes)
    new MCSymbol(MNoAttr, _cd, _name)
  }

  override protected def subterm$(path: Path): MutableCMathML = ???

  override private[cmathml] def extendPath(child: MutableCMathML, childPath: Path): Path = ???
}
final class MCError(attributes: AttributesRO, val cd: String, val name: String, val args: Any*) extends MutableCMathML(attributes) {
  assert(CMathML.isNCName(cd))
  assert(CMathML.isNCName(name))
  override def toCMathML: CMathML = CError(attributesToCMathML,cd,name,args)
  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MutableCMathML = ???

  /** with promise that path!=empty and path.head >= 0 */
  override protected def subterm$(path: Path): MutableCMathML = ???

  override private[cmathml] def extendPath(child: MutableCMathML, childPath: Path): Path = ???
}
object MCError {
  def unapplySeq(that:MCError) = Some((that.cd,that.name,that.args))
}

object MCNone {
  def unapply(that:MCNone) = true
}
final class MCNone(attributes: AttributesRO = MNoAttr) extends MutableCMathML(attributes) with MCILike {
  override def toCMathML: CNone = CNone(attributesToCMathML)
  override def replace(a: MutableCMathML, b: MutableCMathML): Unit =
    replaceInAttributes(a,b)

  override def copy(): MutableCMathML = ???

  override protected def subterm$(path: Path): MutableCMathML = ???

  override private[cmathml] def extendPath(child: MutableCMathML, childPath: Path): Path = ???
}