package info.kwarc.probgen

/** a simple language of expressions, similar to first-order logic with integers as the base type
  */
sealed abstract class Expr {
  def toSTeX: SText
  def toSTeXTop = SMath(toSTeX)
}

object Expr {
  def fromInt(i: Int) = Lit(i)

  def fromAnyO(a: Any): Option[Expr] = {
    try {Some(fromAny(a))}
    catch {case e: Exception => None}
  }
  def fromAny(a: Any): Expr = a match {
    case e: Expr => e
    case i: Int => Lit(i)
    case s: String => NameLit(s)
    case l: List[_] => FinSeq(l.map(fromAny)*)
    case t: Tuple2[_,_] => Tuple(t.productIterator.toList.map(fromAny)*)
  }
}

sealed trait OperApply {
  def op: Oper
  def args: List[Expr]
  def toSTeX = {
    val argsS = args.map(_.toSTeX)
    SMacroApplication(op.stexname, argsS, op.flexary)
  }
  override def toString = {
    if (!op.isInstanceOf[OtherOper] && args.length >= 2) {
      args.mkString("(", " " + op + " ", ")")
    } else {
      op.toString + args.mkString("(", ",", ")")
    }
  }
}

/** formulas */
sealed abstract class Form extends Expr
/** application of a connective to some formulas */
case class Conn(op: COper, args: List[Form]) extends Form with OperApply
/** application of a predicate symbol to some terms */
case class Pred(op: FOper, args: List[Term]) extends Form with OperApply

/** Boolean variables */
case class BVar(name: String) extends Form {
  def toSTeX = SPlainText(name)
  override def toString = name
}

/** terms */
abstract class Term extends Expr
/** application of a function symbol to some terms */
case class Apply(op: TOper, args: List[Term]) extends Term with OperApply
/** reference to a named variable */
case class Var(name: String) extends Term {
  def toSTeX = SPlainText(name)
  override def toString = name
}
/** an integer literal */
case class Lit(value: Int) extends Term {
  override def toString = value.toString
  def toSTeX = SPlainText(toString)
}

/** other expressions like sets, tuples, sequences
  * These can be rendered as stex, but we do not provide computation for them.
  */
abstract class OtherExpr extends Expr
case class OtherApply(op: OtherOper, args: List[Expr]) extends Expr with OperApply
case class NameLit(name: String) extends OtherExpr {
  override def toString = name
  def toSTeX = SPlainText(toString)
}
object NameLit {
  // 0 -> a, 1 -> b, ...
  def apply(i: Int): NameLit = NameLit((97+i).toChar.toString)
}

sealed abstract class Oper {
  override def toString = stexname
  def stexname: String
  def flexary: Boolean
  def minArity: Option[Int] = None
  def maxArity: Option[Int] = None
}

/** predicate symbols */
sealed abstract class FOper(val stexname: String, val flexary: Boolean) extends Oper {
  def apply(args: Term*) = Pred(this, args.toList)
  def unapply(f: Form) = f match {
    case Pred(op,as) if op == this => Some(as)
    case _ => None
  }
}

/** function symbols */
sealed abstract class TOper(val stexname: String, val flexary: Boolean, val arity: Option[Int] = None) extends Oper {
  def apply(args: Term*) = Apply(this, args.toList)
  def unapply(f: Term) = f match {
    case Apply(op,as) if op == this => Some(as)
    case _ => None
  }
  override def minArity = arity
  override def maxArity = arity
}

/** connectives */
sealed abstract class COper(val stexname: String, val flexary: Boolean) extends Oper {
  def apply(args: Form*) = Conn(this, args.toList)
  def unapply(f: Form) = f match {
    case Conn(op,as) if op == this => Some(as)
    case _ => None
  }
}

/** other operators, see [[OtherExpr]] */
sealed abstract class OtherOper(val stexname: String, val flexary: Boolean) extends Oper {
  def apply(args: Expr*): Expr = OtherApply(this, args.toList)
  def apply(is: List[Int]): Expr = apply(is.map(Lit)*)
  def unapply(f: Expr) = f match {
    case OtherApply(op,as) if op == this => Some(as)
    case _ => None
  }
}

object FOper {
  val all = List(Equals, NotEquals, Less, LessEq, Divides)
}
object TOper {
  val all = List(Plus,Times,Minus,Exp)
}

/* individual predicate symbols, function symbols, connectives, etc. */

sealed abstract class ChainedFOper(s: String, f: Boolean) extends FOper(s,f)

/* connectives */
object And extends COper("lconj", true)
object Or extends COper("ldisj", true)
object Implies extends COper("implies", true)
object Neg extends COper("lneg", true)

/* predicate symbols */
object Equals extends ChainedFOper("equals", false)
object NotEquals extends FOper("nequals", false)
object Less extends ChainedFOper("intless", false)
object LessEq extends ChainedFOper("intle", false)
object Divides extends ChainedFOper("divides", false)

/* function symbols */
object Plus extends TOper("intplus", true)
object Minus extends TOper("intminus", true, Some(2))
object Times extends TOper("inttimes", true)
object Exp extends TOper("intpower", true, Some(2))
object Mod extends TOper("intmod", false, Some(2))
object Min extends TOper("intmin", true) {
  override def minArity = Some(2)
}
object Max extends TOper("intmax", true) {
  override def minArity = Some(2)
}

/* others */
object FinSet extends OtherOper("set", true)
object Tuple extends OtherOper("tup", true)
object FinSeq extends OtherOper("seq", true)
object InSet extends OtherOper("inset", false)
case class GivenBy(name: String, args: List[String], df: Expr) extends OtherExpr {
  def toSTeX = SMacroApplication("equals", List(SMacroApplication(name,args.map(SText(_)),true), df.toSTeX), false)
}

/**
  * thrown when evaluation fails
  */
case class EvalError(m: String) extends Exception(m)

/**
  * an evaluator for formulas and terms
  *
  * each evaluation takes a [[Context]] argument that assigns concrete integers to each named variables
  *
  * For example, we can write
  * val F = And(Equals(Var("x"),5), Less(Lit(1), Plus(Var("x"), Var("y")))
  * for the formula F(x,y) = x==5 /\ 1 < x+y
  * and then call
  * Evaluator.apply(F)(Context("x" -> 3)("y" -> 5))
  * to compute F(3,5).
  */
object Evaluator {

  /** evaluates formulas to Booleans */
  def apply(f: Form)(implicit ctx: Context): Boolean = f match {
    case BVar(n) => ctx(n) match {
      case v: Boolean => v
      case v => throw EvalError("variable not integer: " + n + "=" + v)
    }
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
              case Divides => if (prev == 0) false else aE % prev == 0
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

  /** evaluates terms to integers */
  def apply(t: Term)(implicit ctx: Context): Int = t match {
    case Lit(i) => i
    case Var(n) => ctx(n) match {
      case v: Int => v
      case v => throw EvalError("variable not integer: " + n + "=" + v)
    }
    case Apply(op, fs) =>
      val fsE = fs.map(a => apply(a))
      if (op.arity.isEmpty) {
        op match {
          case Plus => fsE.fold(0)((x: Int, y: Int) => x + y)
          case Times => fsE.fold(1)((x: Int, y: Int) => x * y)
          case Min => if (fsE.nonEmpty) fsE.reduce((x: Int, y: Int) => if (x>y) y else x) else 0
          case Max => if (fsE.nonEmpty) fsE.reduce((x: Int, y: Int) => if (x<y) y else x) else 0
          case _ => 0
        }
      } else op match {
        case Minus => fsE(0) - fsE(1)
        case Exp => exp(fsE(0), fsE(1))
        case Mod =>
          val e = fsE(1)
          val m = fsE(0) % e
          if (m < 0) m + e else m
      }
  }
  def exp(a: Int, b: Int): Int = {
    if (b == 0) 1 else a*exp(a,b-1)
  }
}


case class Context(vals: List[(String,AnyVal)]) {
  def apply(n: String) = vals.find(_._1 == n).getOrElse(throw EvalError("undefined variable: " + n))._2
  def apply(v: (String,AnyVal)): Context = Context(v::vals)
}
object Context {
  def apply(v: (String,Int)): Context = Context(List(v))
}