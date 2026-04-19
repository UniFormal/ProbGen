package info.kwarc.probgen

/** a simple language of expressions, similar to first-order logic with integers as the base type
  */

abstract class Domain {
  def apply(v: Any) = Lit(v, this)
}
abstract class IntegerDomain extends Domain
trait OrderedDomain {
  def values: List[Any]
}
case class DUpto(n: Int) extends IntegerDomain with OrderedDomain {
  def values = Range(0,n).toList
}
case object DNat extends IntegerDomain
case object DInt extends IntegerDomain
case object DDouble extends IntegerDomain
case object DString extends Domain
case class DList(elem: Domain) extends Domain
case object DOther extends Domain


sealed abstract class Expr {
  def unary_~ = SMath(this)
  def toSTeX: String
  def toHTML: String
}

trait ExprLike {
  def toExpr: Expr
}

case class Context(vals: List[(String,Any)]) {
  def apply(n: String) = vals.find(_._1 == n).getOrElse(throw EvalError("undefined variable: " + n))._2
  def apply(v: (String,AnyVal)): Context = Context(v::vals)
}
object Context {
  def apply(v: (String,Int)): Context = Context(List(v))
}

implicit class AnyToExpr(a: Any) {
  def unary_! = Expr(a)
}

object Expr {
  implicit def fromInt(i: Int): Term = DInt(i)
  implicit def fromDobule(d: Double): Term = DDouble(d)
  implicit def stringToId(s: String): Var = Var(s)
  def !(a: Any) = apply(a)

  def apply(a: Any) = fromAny(a)
  def fromAnyO(a: Any): Option[Expr] = {
    try {Some(fromAny(a))}
    catch {case e: Exception => None}
  }
  def fromAny(a: Any): Term = a match {
    case e: ExprLike => fromAny(e.toExpr)
    case e: Term => e
    case i: Int => DInt(i)
    case s: String => DString(s)
    case l: Seq[_] => FinSeq(l.map(fromAny)*)
    case s: Set[_] => FinSet(s.toList.map(fromAny)*)
    case t: Tuple2[_,_] => Tuple(t.productIterator.toList.map(fromAny)*)
  }
}

sealed trait OperApply {
  def op: Oper
  def args: Seq[Expr]
  override def toString = {
    if (args.length <= 2) {
      args.mkString("(", " " + op + " ", ")")
    } else {
      op.toString + args.mkString("(", ",", ")")
    }
  }
  def toSTeX = {
    val argsS = args.map(a => SPlainText(a.toSTeX))
    SMacroApplication(op.stexname, argsS, op.flexary).toString
  }
  def toHTML = s"<mrow>${op.toHTML}${args.map(_.toHTML)}</mrow>"
}

/** formulas */
sealed abstract class Form extends Expr
/** application of a connective to some formulas */
case class Conn(op: COper, args: Seq[Form]) extends Form with OperApply
/** application of a predicate symbol to some terms */
case class Pred(op: FOper, args: Seq[Term]) extends Form with OperApply {
  def ===(arg: Term) = op match {
    case op: ChainedFOper => Pred(op, args :+ arg)
    case _ => throw EvalError("not a chained operator")
  }
}

/** Boolean variables */
case class BVar(name: String) extends Form {
  def toSTeX = name
  override def toString = name
  def toHTML = s"<mi>$name</mi>"
}

/** terms */
abstract class Term extends Expr {
  def ===(t: Term) = Equals(this,t)
  def =!=(t: Term) = NotEquals(this,t)
  def apply(args: Term*) = FunApply(this +: args*)
  def +(arg: Term) = Plus(this, arg)
  def *(arg: Term) = Times(this, arg)
  def -(arg: Term) = Minus(this, arg)
  def /(arg: Term) = Divide(this, arg)
  infix def in(arg: Term) = InSet(this,arg)
  infix def to(arg: Term) = RangeSet(this,arg)
}
/** application of a function symbol to some terms */
case class Apply(op: TOper, args: Seq[Term]) extends Term with OperApply

/** application of a big (binding) operator to terms */
case class BigApply(op: BigOper, conds: Seq[Form], body: Term) extends Term {
  def toSTeX = s"\\${op.stexname}_{${conds.map(_.toSTeX).mkString(",\\,")}}{${body.toSTeX}}"
  def toHTML = s"<mrow><munder>${op.mathmlname}<mrow>${conds.map(_.toHTML)}</mrow></munder>${body.toHTML}</mrow>"
}

/** reference to a named variable */
case class Var(name: String) extends Term {
  override def toString = name
  def toSTeX = toString
  def toHTML = s"<mi>$name</mi>"
}
/** an integer literal */
case class Lit(value: Any, domain: Domain) extends Term {
  override def toString = value.toString
  def toSTeX = toString
  def asInt = if (domain == DInt) value.asInstanceOf[Int]
    else throw EvalError("value not an integer: " + this)
  def toHTML = s"<mn>${value.toString}</mn>"
}

object NameLit {
  // 0 -> a, 1 -> b, ...
  def apply(i: Int): Lit = DString((97+i).toChar.toString)
  def applyUpper(i: Int): Lit = DString((65+i).toChar.toString)
}

case class Prob(of: Seq[Expr], conds: Seq[Expr]) extends Term {
  def toSTeX = {
    val ofS = of.map(_.toSTeX).mkString(",\\,")
    val condsS = conds.map(_.toSTeX).mkString(",\\,")
    val (name,args) = if (conds.isEmpty)
      ("uProb", Seq(SPlainText(ofS)))
    else
      ("CondProb", Seq(SPlainText(ofS), SPlainText(condsS)))
    SMacroApplication(name,args,false).toString
  }
  def toHTML = {
    val ofH = s"<mfenced>${of.map(_.toHTML).mkString("")}</mfenced>"
    val argsH = if (conds.isEmpty)
      ofH
    else
      s"$ofH<mo>|</mo><mfenced>${conds.map(_.toHTML).mkString("")}</mfenced>"
    s"<mrow><mo>P</mo>$argsH</mrow>"
  }
}

sealed abstract class Oper {
  def stexname: String
  override def toString = stexname
  def flexary: Boolean
  def minArity: Option[Int] = None
  def maxArity: Option[Int] = None
  def htmlRule: HTMLRule // may be null if toHTML is overridden

  def toHTML(args: Seq[Expr]) = {
    val argsH = args.map(_.toHTML)
    htmlRule match {
      case AppliedOperator(n) => s"<mrow><mo>$n</mo><mfenced>$argsH</mfenced></mrow>"
      case SpecialTag(t) => s"<$t>$argsH</$t>"
      case FencedOperator(o,c) => s"<mfenced open=$o close=$c>$argsH</mfenced>"
    }
  }
}

sealed abstract class BigOper(val stexname: String, val mathmlname: String) {
  def apply(conds: Form*)(body: Term) = BigApply(this, conds, body)
}

/** predicate symbols */
sealed abstract class FOper(val stexname: String, val htmlRule: HTMLRule, val flexary: Boolean) extends Oper {
  def apply(args: Term*) = Pred(this, args.toList)
  def unapply(f: Form) = f match {
    case Pred(op,as) if op == this => Some(as)
    case _ => None
  }
}

/** function symbols */
sealed abstract class TOper(val stexname: String, val htmlRule: HTMLRule, val flexary: Boolean, val arity: Option[Int] = None) extends Oper {
  def apply(args: Term*): Term = Apply(this, args.toList)
  def apply(args: List[Int]): Term = apply(args.map(DInt.apply)*)
  def unapply(f: Term) = f match {
    case Apply(op,as) if op == this => Some(as)
    case _ => None
  }
  override def minArity = arity
  override def maxArity = arity
}

/** connectives */
sealed abstract class COper(val stexname: String, val htmlRule: HTMLRule, val flexary: Boolean) extends Oper {
  def apply(args: Form*) = Conn(this, args.toList)
  def unapply(f: Form) = f match {
    case Conn(op,as) if op == this => Some(as)
    case _ => None
  }
}

object FOper {
  val all = List(Equals, NotEquals, Less, LessEq, Divides)
}
object TOper {
  val all = List(Plus,Times,Minus,Min,Max)
}

/* individual predicate symbols, function symbols, connectives, etc. */

sealed abstract class ChainedFOper(s: String, r: HTMLRule, f: Boolean) extends FOper(s,r,f)

/* connectives */
object And extends COper("lconj", "∧", true)
object Or extends COper("ldisj", "∨", true)
object Implies extends COper("implies", "⇒", true)
object Neg extends COper("lneg", "¬", true)

/* predicate symbols */
object Equals extends ChainedFOper("equals", "=", false)
object NotEquals extends FOper("nequals", "≠", false)
object Less extends ChainedFOper("intlessthan", "<", false)
object LessEq extends ChainedFOper("intlethan", "≤", false)
object Divides extends ChainedFOper("intdivisible", "|", false)
object InSet extends FOper("inset", "∈", false)

/* function symbols */
object Plus extends TOper("intplus", "+", true)
object Minus extends TOper("intminus", "-", true, Some(2))
object Times extends TOper("inttimes", "*", true)
object Divide extends TOper("realdivide", "/", false)
object Exp extends TOper("intpower", SpecialTag("msup"), false, Some(2))
object Mod extends TOper("intmod", "%", false, Some(2))
object Min extends TOper("intmin", "min", true) {
  override def minArity = Some(2)
}
object Max extends TOper("intmax", "max", true) {
  override def minArity = Some(2)
}
object FunApply extends TOper("apply", null, true) {
  override def toHTML(args: Seq[Expr]) = {
    val argsH = args.map(_.toHTML)
    s"""<mrow>${argsH.head}<mfenced open="(" close=")">${argsH.tail}</mfenced></mrow>"""
  }
}
object FinSet extends TOper("set", FencedOperator("{","}"), true)
object RangeSet extends TOper("range", null, false) {
  override def toHTML(args: Seq[Expr]) =
    s"""<mfenced open="{" close="}">${args(0).toHTML}<mo>...</mo>${args(1).toHTML}</mfenced>"""
}
object Tuple extends TOper("tup", FencedOperator("(",")"), true)
object FinSeq extends TOper("seq", FencedOperator("",""), true)
object TransitionChain extends TOper("transitions", null, true) {
  override def toHTML(args: Seq[Expr]) = {
    val argsH = args.map(_.toHTML)
    var left = argsH.tail
    var steps: List[String] = Nil
    while (left.nonEmpty) {
      steps ::= s"<mover>${left(0)}${left(1)}</mover>"
      left = left.drop(2)
    }
    s"<mrow>${argsH(0)}<mover>${steps.reverse}</mrow>"
  }
}

object Sum extends BigOper("sum", "∑")
object Product extends BigOper("times", "∏")
object BigMax extends BigOper("max", "max")
object BigArgMax extends BigOper("argmax", "argmax")
object BigMin extends BigOper("min", "min")

abstract class HTMLRule
case class AppliedOperator(name: String) extends HTMLRule
case class SpecialTag(tag: String) extends HTMLRule
case class FencedOperator(open: String, close: String) extends HTMLRule
object HTMLRule {
  implicit def fromString(s: String): HTMLRule = AppliedOperator(s)
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
      case v => throw EvalError("variable not Boolean: " + n + "=" + v)
    }
    case Pred(op: ChainedFOper, as) => as match {
        case Nil => true
        case hd::tl =>
          var prev = apply(hd).asInt
          tl.forall {a =>
            val aE = apply(a).asInt
            val r = op match {
              case Equals =>
                prev == aE
              case Less => prev < aE
              case LessEq => prev <= aE
              case Divides => if (prev == 0) false else aE % prev == 0
            }
            prev = aE
            r
          }
      }
    case InSet(a::e::_) =>
      val aE = apply(a).asInt
      val eE = apply(e)
      eE.value.asInstanceOf[List[Int]].contains(aE)
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
  def apply(t: Term)(implicit ctx: Context): Lit = t match {
    case l:Lit => l
    case Var(n) => ctx(n) match {
      case v: Int => DInt(v)
      case l: Lit => l
      case v => throw EvalError("variable not integer: " + n + "=" + v)
    }
    case Apply(op, fs) =>
      val fsE = fs.map(a => apply(a).asInt)
      val r = op match {
        case Plus => fsE.fold(0)((x: Int, y: Int) => x + y)
        case Times => fsE.fold(1)((x: Int, y: Int) => x * y)
        case Min => if (fsE.nonEmpty) fsE.reduce((x: Int, y: Int) => if (x>y) y else x) else 0
        case Max => if (fsE.nonEmpty) fsE.reduce((x: Int, y: Int) => if (x<y) y else x) else 0
        case Minus => fsE(0) - fsE(1)
        case Divide => fsE(0)/fsE(1)
        case Exp => exp(fsE(0), fsE(1))
        case Mod =>
          val e = fsE(1)
          val m = fsE(0) % e
          if (m < 0) m + e else m
      }
      DInt(r)
  }
  def exp(a: Int, b: Int): Int = {
    if (b == 0) 1 else a*exp(a,b-1)
  }
}
