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

  def toSTeX = op.sTeX(args.map(_.toSTeX))

  def toHTML = op.toHTML(rendered(_.toHTML))

  /** brackets a nested bare operator, so that Times(Plus(x,y),z) is not shown as x + y * z */
  private def rendered(render: Expr => String): Seq[String] =
    args.map {
      case a: OperApply if op.bracketsChildren && op.htmlRule.isBare && a.op.htmlRule.isBare =>
        "(" + render(a) + ")"
      case a => render(a)
    }
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
  override def toString = name
  def toSTeX = name
  def toHTML = s"<i>$name</i>"
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
  def toHTML = s"${op.sym}<sub>${conds.map(_.toHTML).mkString(", ")}</sub>(${body.toHTML})"
}

object Name {
  private val symbols = Map(
    "\\gamma"  -> "γ", "\\pi"    -> "π", "\\sigma" -> "σ", "\\mu"     -> "μ",
    "\\alpha"  -> "α", "\\beta"  -> "β", "\\theta" -> "θ", "\\lambda" -> "λ",
    "\\to"     -> "→", "\\times" -> "×"
  )
  private val upright = Set("→", "×")

  private def unwrap(s: String, macroName: String): Option[String] = {
    val open = "\\" + macroName + "{"
    if (s.startsWith(open) && s.endsWith("}")) Some(s.stripPrefix(open).stripSuffix("}")) else None
  }

  def html(s: String): String =
    symbols.get(s).map(g => if (upright.contains(g)) s"<span class='sym'>$g</span>" else s"<i>$g</i>")
      .orElse(unwrap(s, "mathtt").map(t => s"<span class='mathtt'>$t</span>"))
      .orElse(unwrap(s, "mathrm"))
      .getOrElse(s"<i>$s</i>")
}

/** reference to a named variable */
case class Var(name: String) extends Term {
  override def toString = name
  def toSTeX = toString
  def toHTML = Name.html(name)
}
/** an integer literal */
case class Lit(value: Any, domain: Domain) extends Term {
  override def toString = value.toString
  def toSTeX = toString
  def asInt = if (domain == DInt) value.asInstanceOf[Int]
    else throw EvalError("value not an integer: " + this)

  def toHTML = value match {
    case inner: Lit => inner.toHTML
    case _ => domain match {
      case DString => Name.html(value.toString)
      case _       => s"<span class='num'>${value.toString}</span>"
    }
  }
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
    val ofH = of.map(_.toHTML).mkString(", ")
    if (conds.isEmpty) s"<i>P</i>($ofH)"
    else s"<i>P</i>($ofH | ${conds.map(_.toHTML).mkString(", ")})"
  }
}

abstract class HTMLRule {
  def isBare: Boolean = false
}
/** `a + b`; the symbol is given bare, spacing is added when rendering */
case class Infix(html: String) extends HTMLRule {
  override def isBare = true
}
/** `¬a` */
case class Prefix(html: String) extends HTMLRule {
  override def isBare = true
}
/** `min(a, b)` */
case class AppliedOperator(name: String) extends HTMLRule
/** `{a, b}`, `(a, b)`, or just `a, b` with empty fences */
case class FencedOperator(open: String, close: String) extends HTMLRule
/** the operator overrides toHTML itself */
case object CustomRule extends HTMLRule

object HTMLRule {
  implicit def fromString(s: String): HTMLRule = Infix(s)
}

sealed abstract class Oper {
  def stexname: String
  override def toString = stexname
  def flexary: Boolean
  def minArity: Option[Int] = None
  def maxArity: Option[Int] = None

  def htmlRule: HTMLRule

  /** whether nested bare operators among the arguments get brackets; see [OperApply] */
  def bracketsChildren: Boolean = true

  def sTeX(args: Seq[String]): String =
    SMacroApplication(stexname, args.map(SPlainText(_)), flexary).toString

  def toHTML(args: Seq[String]): String = htmlRule match {
    case Infix(h)            => args.mkString(" " + h + " ")
    case Prefix(h)           => h + args.mkString
    case AppliedOperator(n)  => n + args.mkString("(", ", ", ")")
    case FencedOperator(o,c) => o + args.mkString(", ") + c
    case CustomRule          => throw EvalError("no HTML rendering for " + stexname)
  }
}

sealed abstract class BigOper(val stexname: String, val sym: String) {
  def apply(conds: Form*)(body: Term) = BigApply(this, conds, body)
}

/** predicate symbols */
sealed abstract class FOper(val stexname: String, val htmlRule: HTMLRule, val flexary: Boolean) extends Oper {
  override def bracketsChildren = false
  def apply(args: Term*) = Pred(this, args.toList)
  def unapply(f: Form) = f match {
    case Pred(op,as) if op == this => Some(as)
    case _ => None
  }
}

/** function symbols */
sealed abstract class TOper(val stexname: String, val htmlRule: HTMLRule, val flexary: Boolean, val arity: Option[Int]) extends Oper {
  def this(stexname: String, htmlRule: HTMLRule, flexary: Boolean) =
    this(stexname, htmlRule, flexary, None)

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

// lazy on purpose: these objects refer back to the operators, so building them
// eagerly risks the initialisation cycle described at TOper's auxiliary constructor
object FOper {
  lazy val all = List(Equals, NotEquals, Less, LessEq, Divides)
}
object TOper {
  lazy val all = List(Plus,Times,Minus,Min,Max)
}

/* individual predicate symbols, function symbols, connectives, etc. */

sealed abstract class ChainedFOper(s: String, r: HTMLRule, f: Boolean) extends FOper(s,r,f)

/* connectives */
object And extends COper("lconj", Infix("∧"), true)
object Or extends COper("ldisj", Infix("∨"), true)
object Implies extends COper("limpl", Infix("⇒"), false)
object Neg extends COper("lneg", Prefix("¬"), true)

/* predicate symbols */
object Equals extends ChainedFOper("eq", "=", true)
object NotEquals extends FOper("notequal", Infix("≠"), false)
object Less extends ChainedFOper("intlessthan", Infix("&lt;"), false)
object LessEq extends ChainedFOper("intlethan", Infix("≤"), false)
object Divides extends ChainedFOper("intdivisible", "|", false)
object InSet extends FOper("inset", Infix("∈"), false)

/* function symbols */
object Plus extends TOper("intplus", "+", true)
object Minus extends TOper("intminus", Infix("−"), true, Some(2))
object Times extends TOper("inttimes", Infix("·"), true)
object Mod extends TOper("intmod", "mod", false, Some(2))
object Min extends TOper("intmin", AppliedOperator("min"), true) {
  override def minArity = Some(2)
}
object Max extends TOper("intmax", AppliedOperator("max"), true) {
  override def minArity = Some(2)
}

object Cart extends TOper("cart", Infix("×"), true)
object FinSet extends TOper("set", FencedOperator("{","}"), true)
object Tuple extends TOper("tup", FencedOperator("(",")"), true)
object FinSeq extends TOper("seq", FencedOperator("",""), true)

/* function symbols whose layout is more than a symbol between the arguments */
object Divide extends TOper("realdivide", CustomRule, false) {
  override def toHTML(a: Seq[String]) =
    if (a.length >= 2) s"<span class='frac'><span>${a(0)}</span><span>${a(1)}</span></span>"
    else a.mkString(" / ")
}
object Exp extends TOper("intpower", CustomRule, false, Some(2)) {
  override def toHTML(a: Seq[String]) = s"${a(0)}<sup>${a(1)}</sup>"
}
object FunApply extends TOper("apply", CustomRule, true) {
  override def sTeX(a: Seq[String]) = s"\\apply{${a.head}}{${a.tail.mkString(",")}}"
  override def toHTML(a: Seq[String]) = s"${a.head}(${a.tail.mkString(", ")})"
}
object RangeSet extends TOper("range", CustomRule, false) {
  override def toHTML(a: Seq[String]) = s"{${a(0)}, …, ${a(1)}}"
}
/** a path: a state, then alternating action and state, as produced by [[Path.toExpr]] */
object TransitionChain extends TOper("transitions", CustomRule, true) {
  override def toHTML(a: Seq[String]) =
    a.head + a.tail.grouped(2).map {
      case Seq(act, st) => s" →<sub>$act</sub> $st"
      case Seq(st)      => s" → $st"
      case _            => ""
    }.mkString("")
}

object Sum extends BigOper("sum", "Σ")
object Product extends BigOper("prod", "Π")
object BigMax extends BigOper("max", "max")
object BigArgMax extends BigOper("argmax", "argmax")
object BigMin extends BigOper("min", "min")

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
