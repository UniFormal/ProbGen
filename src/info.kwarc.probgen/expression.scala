package info.kwarc.probgen

case class Context(vals: List[(String,Int)]) {
  def apply(n: String) = vals.find(_._1 == n).get._2
  def apply(v: (String,Int)): Context = Context(v::vals)
}
object Context {
  def apply(v: (String,Int)): Context = Context(List(v))
}

sealed abstract class Expr {
  def toSTeX: SText
  def toSTeXTop = SMath(toSTeX)
}

sealed trait OperApply {
  def op: Oper
  def args: List[Expr]
  def toSTeX = {
    val argsS = args.map(_.toSTeX)
    SMacroApplication(op.stexname, argsS, op.flexary)
  }
  override def toString = {
    if (args.length == 2) {
      "(" + args(0) + " " + op + " " + args(1) + ")"
    } else {
      op + args.mkString("(", ",", ")")
    }
  }
}

sealed abstract class Form extends Expr
case class Conn(op: COper, args: List[Form]) extends Form with OperApply
case class Pred(op: FOper, args: List[Term]) extends Form with OperApply

abstract class Term extends Expr
case class Apply(op: TOper, args: List[Term]) extends Term with OperApply
case class Var(name: String) extends Term {
  def toSTeX = SPlainText(name)
  override def toString = name
}
case class Lit(value: Int) extends Term {
  override def toString = value.toString
  def toSTeX = SPlainText(toString)
}

abstract class OtherExpr extends Expr
case class OtherApply(op: OtherOper, args: List[Expr]) extends Expr with OperApply
case object Ellipsis {
  def toSTeX = "\\ldots"
}

sealed abstract class Oper {
  override def toString = stexname
  def stexname: String
  def flexary: Boolean
  def minArity: Option[Int] = None
  def maxArity: Option[Int] = None
}

sealed abstract class FOper(val stexname: String, val flexary: Boolean) extends Oper {
  def unapply(f: Form) = f match {
    case Pred(op,as) if op == this => Some(as)
    case _ => None
  }
}

sealed abstract class TOper(val stexname: String, val flexary: Boolean) extends Oper {
  def unapply(f: Term) = f match {
    case Apply(op,as) if op == this => Some(as)
    case _ => None
  }
}

sealed abstract class COper(val stexname: String, val flexary: Boolean) extends Oper {
  def unapply(f: Form) = f match {
    case Conn(op,as) if op == this => Some(as)
    case _ => None
  }
}

sealed abstract class OtherOper(val stexname: String, val flexary: Boolean) extends Oper {
  def apply(args: Expr*) = OtherApply(this, args.toList)
  def unapply(f: Expr) = f match {
    case OtherApply(op,as) if op == this => Some(as)
    case _ => None
  }
}

object FOper {
  val all = List(Equals, NotEquals, Less, LessEq)
}
object TOper {
  val all = List(Plus,Times,Minus,Exp)
}

sealed abstract class ChainedFOper(s: String, f: Boolean) extends FOper(s,f)

object And extends COper("lconj", true)
object Or extends COper("ldisj", true)
object Implies extends COper("implies", true)
object Neg extends COper("lneg", true)

object Equals extends ChainedFOper("equals", false)
object NotEquals extends FOper("nequals", false)
object Less extends ChainedFOper("intless", false)
object LessEq extends ChainedFOper("intle", false)

object Plus extends TOper("intplus", true)
object Minus extends TOper("intminus", true)
object Times extends TOper("inttimes", true)
object Exp extends TOper("intpower", true)

object FinSet extends OtherOper("set", true)
object Tuple extends OtherOper("tup", true)

object Evaluator {

  def apply(f: Form)(implicit ctx: Context): Boolean = f match {
    case Pred(op: ChainedFOper, as) => as match {
        case Nil => true
        case hd::tl =>
          var prev = apply(hd)
          tl.forall {a =>
            val aE = apply(a)
            val r = op match {
              case Equals => prev == aE
              case Less => prev < aE
              case LessEq => prev <= aE
            }
            prev = aE
            r
          }
      }
    case NotEquals(as) =>
      val asE = as.map(a => apply(a))
      asE.length == asE.distinct.length
    case Implies(as) =>
      val asE = as.map(a => apply(a))
      asE match {
        case Nil => false
        case l => l.init.exists(a => !a) || l.last
      }
    case Neg(as) => as.exists(a => !apply(a))
    case And(as) => as.forall(a => apply(a))
    case Or(as) => as.exists(a => apply(a))
  }

  def apply(t: Term)(implicit ctx: Context): Int = t match {
    case Lit(i) => i
    case Var(n) => ctx(n)
    case Apply(op, fs) =>
      val fsE = fs.map(a => apply(a))
      val (neut, fold): (Int, (Int,Int) => Int) = op match {
        case Plus => (0,(x,y) => x+y)
        case Times => (1,(x,y) => x*y)
        case Minus => (0,(x,y) => x-y)
        case Exp => (1,(x,y) => x^y)
      }
      fsE.fold(neut)(fold)
  }
}