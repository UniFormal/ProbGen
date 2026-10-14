package info.kwarc.probgen

/* a simple language of expressions, similar to first-order logic with various built-in base types */

/** all expressions including formulas and terms */
sealed abstract class Expr {
  def unary_~ = SMath(this)
  def toSTeX: String
  def toHTML: String
  def fvs: Seq[String]
  def fvsD = fvs.distinct.sorted
}

object Expr {
  implicit def fromInt(i: Int): Term = DInt(i)
  implicit def fromDouble(d: Double): Term = DDouble(d)
  implicit def stringToId(s: String): Var = Var(s)
  def !(a: Any) = apply(a)

  def apply(a: Any) = fromAny(a)
  def fromAnyO(a: Any): Option[Term] = {
    try {Some(fromAny(a))}
    catch {case e: Exception => None}
  }
  def fromAny(a: Any): Term = a match {
    case e: Term => e
    case e: ExprLike => e.toExpr
    case i: Int => DInt(i)
    case d: Double => DDouble(d)
    case s: String => DString(s)
    case l: Seq[_] => FinSeq(l.map(fromAny)*)
    case s: Set[_] => FinSet(s.toList.map(fromAny)*)
    case t: Tuple2[_,_] => Tuple(t.productIterator.toList.map(fromAny)*)
  }

  def toHTML(es: Seq[Expr], sep: String = "", open: String = "", close: String = "") = {
    val oS = if (open.isEmpty) "" else s"<mo>$open</mo>"
    val cS = if (close.isEmpty) "" else s"<mo>$close</mo>"
    val sS = if (sep.isEmpty) "" else s"<mo>$sep</mo>"
    es.map(_.toHTML).mkString(oS, sS, cS)
  }
}

trait ExprLike {
  def toExpr: Term
}

implicit class AnyToExpr(a: Any) {
  def unary_! = Expr(a)
}

/** built-in literals */
abstract class Domain
abstract class IntegerDomain extends Domain {
  def apply(v: Int) = Lit(v, this)
}
trait OrderedDomain {
  def values: List[Any]
}
case class DUpto(n: Int) extends IntegerDomain with OrderedDomain {
  def values = Range(0,n).toList
}
case object DNat extends IntegerDomain
case object DInt extends IntegerDomain
case object DDouble extends Domain {
  def apply(v: Double) = Lit(v, this)
}
case object DString extends Domain {
  def apply(v: String) = Lit(v, this)
}
case object DOther extends Domain

/** common parent of all applications of constants/operators/connectives */
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
  def toSTeX = SMacroApplication(op.stexname, args.map(a => SPlainText(a.toSTeX)), op.flexary).toString
  def toHTML = op.toHTML(args)
  def fvs = args.flatMap(_.fvs)
}

/** common parent of variables */
sealed trait AnyVar {
  val name: String
  override def toString = name
  def toSTeX = name
  def toHTML = Name.html(name)
  def fvs = List(name)
}

/** common parent of literals */
sealed trait AnyLit {
  val value: Any
  override def toString = value.toString
  def fvs = Nil
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
case class BVar(name: String) extends Form with AnyVar
/** Boolean literals */
case class BLit(value: Boolean) extends Form with AnyLit {
  def toSTeX = if (value) "\\semtrue" else "\\semfalse"
  def toHTML = s"<mi>${if (value) "true" else "false"}</mi>"
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
case class BigApply(op: BigOper, bnds: List[String], conds: Seq[Form], body: Term) extends Term {
  def toSTeX = s"\\${op.stexname}_{${conds.map(_.toSTeX).mkString(",\\,")}}{${body.toSTeX}}"
  def toHTML = {
    val bodyH = body match {
      case o: OperApply if o.op.precedence < Oper.precedence(Times) => Oper.bracket(body.toHTML)
      case _ => body.toHTML
    }
    s"<mrow><munder><mo>${op.sym}</mo><mrow>${Expr.toHTML(conds,",")}</mrow></munder>$bodyH</mrow>"
  }
  def fvs = body.fvs.filterNot(bnds.contains)
}

/** reference to a named variable */
case class Var(name: String) extends Term with AnyVar

/** an integer literal */
case class Lit(value: Any, domain: Domain) extends Term with AnyLit {
  def toSTeX = toString
  def asInt = if (domain == DInt) value.asInstanceOf[Int] else throw EvalError("value not an integer: " + this)
  def toHTML = domain match {
    case DString => Name.html(value.toString)
    case _       => Name.number(value.toString)
  }
}

/** probability */
case class Prob(of: Seq[Expr], conds: Seq[Expr]) extends Term {
  def toSTeX = {
    val ofS = of.map(_.toSTeX).mkString(",\\,")
    val condsS = conds.map(_.toSTeX).mkString(",\\,")
    val (name,args) = if (conds.isEmpty)
      ("uProb", Seq(SPlainText(ofS)))
    else
      ("CondProb", Seq(SPlainText(ofS), SPlainText(condsS)))
    SMacroApplication(name,args,-1).toString
  }
  def toHTML = {
    val condsH = if (conds.isEmpty) "" else "<mo>|</mo>" + Expr.toHTML(conds,",")
    s"""<mrow><mi>P</mi><mo>(</mo>${Expr.toHTML(of,",")}$condsH<mo>)</mo></mrow>"""
  }
  /** TODO check */
  def fvs = of.flatMap(_.fvs) ++ conds.flatMap(_.fvs)
}


object Name {
  private val NumberR = "-?\\d+(\\.\\d+)?".r
  private val SingleCommand = "\\\\([A-Za-z]+)".r
  private val Subscript = "([^_]+)_\\{?([^{}]+)\\}?".r

  def html(s: String): String = {
    val t = s.trim
    if (NumberR.matches(t)) number(t)
    else unwrap(t, "mathtt").map(n => s"""<mi mathvariant="normal" class="mathtt">${Html.esc(n)}</mi>""")
      .orElse(unwrap(t, "mathrm").map(n => s"""<mi mathvariant="normal">${Html.esc(n)}</mi>"""))
      .orElse(primed(t))
      .orElse(subscripted(t))
      .getOrElse(plain(t))
  }

  /** a numeric literal; negative numbers get a minus operator like in LaTeX */
  def number(n: String): String =
    if (n.startsWith("-")) s"<mrow><mo>−</mo><mn>${n.drop(1)}</mn></mrow>" else s"<mn>$n</mn>"

  private def unwrap(s: String, macroName: String): Option[String] = {
    val open = "\\" + macroName + "{"
    if (s.startsWith(open) && s.endsWith("}")) Some(s.stripPrefix(open).stripSuffix("}")) else None
  }

  private def primed(t: String): Option[String] = {
    val base = t.reverse.dropWhile(_ == '\'').reverse
    val n = t.length - base.length
    if (n == 0 || base.isEmpty) None
    else Some(s"<msup>${html(base)}<mo>${"′" * n}</mo></msup>")
  }

  private def subscripted(t: String): Option[String] = t match {
    case Subscript(base, sub) => Some(s"<msub>${html(base)}${html(sub)}</msub>")
    case _                    => None
  }

  private def plain(t: String): String = t match {
    case SingleCommand(c) if Symbols.unicode(c).isDefined =>
      val u = Symbols.unicode(c).get
      if (Symbols.isIdentifier(u)) s"<mi>$u</mi>" else s"<mo>$u</mo>"
    case _ => s"<mi>${Html.esc(Symbols.replaceCommands(t))}</mi>"
  }
}

object NameLit {
  // 0 -> a, 1 -> b, ...
  def apply(i: Int): Lit = DString((97+i).toChar.toString)
  def applyUpper(i: Int): Lit = DString((65+i).toChar.toString)
}

sealed abstract class HTMLRule
case class Infix(name: String) extends HTMLRule
case class Prefix(name: String) extends HTMLRule
case class SpecialTag(tag: String) extends HTMLRule
case class AppliedOperator(name: String) extends HTMLRule
case class FencedOperator(open: String, close: String) extends HTMLRule

object HTMLRule {
  implicit def fromString(s: String): HTMLRule = Infix(s)
}

sealed abstract class Oper {
  def stexname: String
  override def toString = stexname
  def flexary: Int
  def minArity: Option[Int] = None
  def maxArity: Option[Int] = None

  /** how strongly this operator binds, see [[Oper.precedence]] */
  def precedence: Int = Oper.precedence(this)

  def htmlRule: HTMLRule // may be null if toHTML is overridden
  def toHTML(args: Seq[Expr]): String = {
    htmlRule match {
      case Infix(n) => s"<mrow>${args.map(argHTML(_, false)).mkString(s"<mo>$n</mo>")}</mrow>"
      case Prefix(n) => s"<mrow><mo>$n</mo>${args.map(argHTML(_, true)).mkString}</mrow>"
      case SpecialTag(t) =>
        val parts = args.zipWithIndex.map { case (a, i) => if (t == "msup" && i == 0) argHTML(a, false) else a.toHTML }
        s"<$t>${parts.map(p => s"<mrow>$p</mrow>").mkString}</$t>"
      case AppliedOperator(n) => s"<mrow><mo>$n</mo>${Expr.toHTML(args, ",", "(", ")")}</mrow>"
      case FencedOperator(o,c) => s"<mrow>${Expr.toHTML(args, ",", o, c)}</mrow>"
    }
  }

  private def argHTML(a: Expr, prefix: Boolean): String = {
    val needed = a match {
      case o: OperApply =>
        val p = o.op.precedence
        p < precedence || (!prefix && p == precedence && !(o.op == this && Oper.associative(this)))
      case _ => false
    }
    if (needed) Oper.bracket(a.toHTML) else a.toHTML
  }
}

object Oper {
  /** binding strength for MathML rendering: higher binds tighter */
  def precedence(op: Oper): Int = op match {
    case Implies      => 10
    case Or           => 20
    case And          => 30
    case Neg          => 40
    case _: FOper     => 50
    case Plus | Minus => 60
    case Mod          => 70
    case Times | Cart => 80
    case Exp          => 90
    case _            => 100
  }

  def associative(op: Oper): Boolean = op match {
    case And | Or | Plus | Times => true
    case _                       => false
  }

  def bracket(html: String): String = s"<mrow><mo>(</mo>$html<mo>)</mo></mrow>"
}

sealed abstract class BigOper(val stexname: String, val sym: String) {
  def apply(bnds: List[String])(conds: Form*)(body: Term): BigApply = {
    BigApply(this, bnds, conds, body)
  }
  def apply(n: String, rng: Term)(body: Term): BigApply = {
    this(List(n))(InSet(Var(n),rng))(body)
  }
}

/** predicate symbols */
sealed abstract class FOper(val stexname: String, val htmlRule: HTMLRule, val flexary: Int) extends Oper {
  def apply(args: Term*) = Pred(this, args.toList)
  def unapply(f: Form) = f match {
    case Pred(op,as) if op == this => Some(as)
    case _ => None
  }
}

/** function symbols */
sealed abstract class TOper(val stexname: String, val htmlRule: HTMLRule, val flexary: Int, val arity: Option[Int])
  extends Oper {
  def this(stexname: String, htmlRule: HTMLRule, flexary: Int) = this(stexname, htmlRule, flexary, None)

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
sealed abstract class COper(val stexname: String, val htmlRule: HTMLRule, val flexary: Int) extends Oper {
  def apply(args: Form*): Conn = Conn(this, args.toList)
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

sealed abstract class ChainedFOper(s: String, r: HTMLRule, f: Int) extends FOper(s,r,f)

/* connectives */
object And extends COper("lconj", Infix("∧"), 0)
object Or extends COper("ldisj", Infix("∨"), 0)
object Implies extends COper("limpl", Infix("⇒"), -1)
object Neg extends COper("lneg", Prefix("¬"), 0)

/* predicate symbols */
object Equals extends ChainedFOper("eq", "=", 0)
object NotEquals extends FOper("notequal", Infix("≠"), -1)
object Less extends ChainedFOper("intlessthan", Infix("&lt;"), -1)
object LessEq extends ChainedFOper("intlethan", Infix("≤"), -1)
object Divides extends ChainedFOper("intdivisible", "|", -1)
object InSet extends FOper("inset", Infix("∈"), -1)

/* function symbols */
object Plus extends TOper("intplus", "+", 0)
object Minus extends TOper("intminus", Infix("−"), 0, Some(2))
object Times extends TOper("inttimes", Infix("·"), 0)
object Mod extends TOper("intmod", "mod", -1, Some(2))
object Min extends TOper("intmin", AppliedOperator("min"), 0) {
  override def minArity = Some(2)
}
object Max extends TOper("intmax", AppliedOperator("max"), 0) {
  override def minArity = Some(2)
}

object Cart extends TOper("cart", Infix("×"), 0)
object FinSet extends TOper("set", FencedOperator("{","}"), 0)
object Tuple extends TOper("tup", FencedOperator("⟨","⟩"), 0) 
object FinSeq extends TOper("seq", FencedOperator("",""), 0)

/* function symbols whose layout is more than a symbol between the arguments */
object Divide extends TOper("realdivide", SpecialTag("mfrac"), -1)
object Exp extends TOper("intpower", SpecialTag("msup"), -1, Some(2))
object FunApply extends TOper("apply", null, 1) {
  override def toHTML(args: Seq[Expr]) = {
    s"""<mrow>${args.head.toHTML}${Expr.toHTML(args.tail, ",", "(", ")")}</mrow>"""
  }
}
object RangeSet extends TOper("range", null, -1) {
  override def toHTML(args: Seq[Expr]) =
    s"""<mrow><mo>{</mo>${args(0).toHTML}<mo>,</mo><mo>…</mo><mo>,</mo>${args(1).toHTML}<mo>}</mo></mrow>"""
}
/** a path: a state, then alternating action and state, as produced by [[Path.toExpr]] */
object TransitionChain extends TOper("transitions", null, 0) {
  override def toHTML(args: Seq[Expr]) = {
    val steps = args.tail.grouped(2).map { step =>
      val arrow = s"<mover><mo>→</mo>${step.head.toHTML}</mover>"
      arrow + step.tail.map(_.toHTML).mkString
    }.mkString
    s"<mrow>${args.head.toHTML}$steps</mrow>"
  }
}

object Sum extends BigOper("sum", "Σ")
object Product extends BigOper("prod", "Π")
object BigMax extends BigOper("max", "max")
object BigArgMax extends BigOper("argmax", "argmax")
object BigMin extends BigOper("min", "min")

/** assigns values to free variables */
case class Context(vals: List[(String,Expr)]) {
  def apply(n: String) = vals.find(_._1 == n).getOrElse(throw EvalError("undefined variable: " + n))._2
  def apply(v: (String,AnyVal)): Context = Context((v._1, Expr(v._2))::vals)
  def declares(n: String) = vals.exists(_._1 == n)
  override def toString = vals.map(v => v._1 + "=" + v._2.toString).mkString(", ")
}
object Context {
  def apply(): Context = Context(Nil)
  def apply(v: (String,Int)): Context = Context()(v)
}

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
    case BLit(v) => v
    case BVar(n) => ctx(n) match {
      case v: Form => apply(v)
      case v => throw EvalError("variable not a formula: " + n + "=" + v)
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
      case t: Term => apply(t)
      case v => throw EvalError("variable not a term: " + n + "=" + v)
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
