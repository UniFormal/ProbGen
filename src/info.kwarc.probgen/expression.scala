package info.kwarc.probgen

/* a simple language of expressions, similar to higher-order logic with various built-in base types */

/** types */
abstract class Domain
/** finite domain with a canonical ordering of its elements, e.g., to write a finite maps as vectors */
trait OrderedDomain {
  def values: List[Any]
}

/** an abstract domain, e.g., for FOL */
case object AbstractDomain extends Domain

/** built-in types with literals */
abstract class LiteralDomain extends Domain

/* concrete literal domains */

abstract class IntegerDomain extends LiteralDomain {
  def apply(v: Int) = Lit(v, this)
}
case class DUpto(n: Int) extends IntegerDomain with OrderedDomain {
  def values = Range(0,n).toList
}
case object DNat extends IntegerDomain
case object DInt extends IntegerDomain

case object DDouble extends LiteralDomain {
  def apply(v: Double) = Lit(v, this)
}
case object DBool extends LiteralDomain {
  def apply(v: Boolean) = Lit(v, this)
}
case object DString extends LiteralDomain {
  def apply(v: String) = Lit(v, this)
}

/* complex types */

/** cartesian product of types */
case class CartProd(doms: List[Domain]) extends Domain
/** lists over a type */
case class ListDomain(elem: Domain) extends Domain
/** the type of sets */
case object SetDomain extends Domain
/** the type of elements of an expression that represents a set */
case class ElementsOf(set: Expr) extends Domain
/** power type */
case class SubsetOf(elem: Domain) extends Domain
/** the type of contexts with certain fields, i.e., record types */
case class ContextType(doms: List[(String,Domain)]) extends Domain

/**  expressions, formulas are expressions of type DBool */
sealed abstract class Expr {
  def unary_~ = SMath(this)
  def toSTeX: String
  def toHTML: String
  def fvs: Seq[String]
  def fvsD = fvs.distinct.sorted

  /* convenience functions for creating expressions programmatically */
  def ===(t: Expr) = Equals(this,t)
  def =!=(t: Expr) = NotEquals(this,t)
  def apply(args: Expr*) = FunApply(this +: args*)
  def +(arg: Expr) = Plus(this, arg)
  def *(arg: Expr) = Times(this, arg)
  def -(arg: Expr) = Minus(this, arg)
  def /(arg: Expr) = Divide(this, arg)
  infix def in(arg: Expr) = InSet(this,arg)
  infix def to(arg: Expr) = RangeSet(this,arg)
}

object Expr {
  implicit def fromInt(i: Int): Expr = DInt(i)
  implicit def fromDouble(d: Double): Expr = DDouble(d)
  implicit def stringToId(s: String): Var = Var(s)
  def !(a: Any) = apply(a)

  def apply(a: Any) = fromAny(a)
  def fromAnyO(a: Any): Option[Expr] = {
    try {Some(fromAny(a))}
    catch {case e: Exception => None}
  }
  def fromAny(a: Any): Expr = a match {
    case e: Expr => e
    case e: ExprLike => e.toExpr
    case b: Boolean => DBool(b)
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
  def toExpr: Expr
}

implicit class AnyToExpr(a: Any) {
  def unary_! = Expr(a)
}

/** variable references */
case class Var(name: String) extends Expr {
  override def toString = name
  def toSTeX = name
  def toHTML = Name.html(name)
  def fvs = List(name)
}

/** common parent of literals */
case class Lit(value: Any, domain: LiteralDomain) extends Expr {
  override def toString = value.toString
  def asInt = if (domain == DInt) value.asInstanceOf[Int] else throw EvalError("value not an integer: " + this)
  def asDouble = if (domain == DInt) value.asInstanceOf[Int].toDouble else
    if (domain == DDouble) value.asInstanceOf[Double] else throw EvalError("value not an integer: " + this)
  def asBool = if (domain == DBool) value.asInstanceOf[Boolean] else throw EvalError("value not a Boolean: " + this)
  def asString = if (domain == DString) value.asInstanceOf[String] else throw EvalError("value not a string: " + this)
  def fvs = Nil
  def toSTeX = domain match {
    case DBool => if (asBool) "\\semtrue" else "\\semfalse"
    case _ => value.toString
  }
  def toHTML = domain match {
    case DBool => s"<mi>${if (asBool) "true" else "false"}</mi>"
    case DString => Name.html(value.toString)
    case _:IntegerDomain | DDouble => Name.number(value.toString)
  }
}


/** common parent of all applications of constants/operators/connectives */
case class OperApply(op: Oper, args: Seq[Expr]) extends Expr {
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

  override def ===(arg: Expr) = op match {
    case op: ChainedFOper => OperApply(op, args :+ arg)
    case _ => super.===(arg)
  }
}

/** application of a big (binding) operator to terms */
case class BigApply(op: BigOper, bnds: List[String], conds: Seq[Expr], body: Expr) extends Expr {
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

/** typed quantification */
case class QuantifiedForm(univ: Boolean, vr: String, dom: Domain, body: Expr) extends Expr {
  def fvs = body.fvs.diff(List(vr))
  def toHTML = ??? // TODO
  def toSTeX = ??? // TODO
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

/** operators */
sealed abstract class Oper {
  def stexname: String
  override def toString = stexname
  def flexary: Int
  def minArity: Option[Int] = None
  def maxArity: Option[Int] = None

  def apply(args: Expr*) = OperApply(this,args.toList)
  def unapply(f: Expr) = f match {
    case OperApply(op,as) if op == this => Some(as)
    case _ => None
  }

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
    case MapsTo       => 5
    case Implies      => 10
    case Or           => 20
    case And          => 30
    case Neg          => 40
    case _: FOper     => 50
    case Plus | Minus | Union | SetDiff => 60
    case Mod          => 70
    case Times | Cart | Intersection => 80
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
  def apply(bnds: List[String])(conds: Expr*)(body: Expr): BigApply = {
    BigApply(this, bnds, conds, body)
  }
  def apply(n: String, rng: Expr)(body: Expr): BigApply = {
    this(List(n))(InSet(Var(n),rng))(body)
  }
}

/** rules for HTML rendering operator applications */
sealed abstract class HTMLRule
case class Infix(name: String) extends HTMLRule
case class Prefix(name: String) extends HTMLRule
case class SpecialTag(tag: String) extends HTMLRule
case class AppliedOperator(name: String) extends HTMLRule
case class FencedOperator(open: String, close: String) extends HTMLRule

object HTMLRule {
  implicit def fromString(s: String): HTMLRule = Infix(s)
}

/* concrete operators */

/** predicate symbols */
sealed abstract class FOper(val stexname: String, val htmlRule: HTMLRule, val flexary: Int) extends Oper

/** function symbols */
sealed abstract class TOper(val stexname: String, val htmlRule: HTMLRule, val flexary: Int, val arity: Option[Int])
  extends Oper {
  def this(stexname: String, htmlRule: HTMLRule, flexary: Int) = this(stexname, htmlRule, flexary, None)
  def apply(args: List[Int]): Expr = apply(args.map(DInt.apply)*)
  override def minArity = arity
  override def maxArity = arity
}

/** connectives */
sealed abstract class COper(val stexname: String, val htmlRule: HTMLRule, val flexary: Int) extends Oper

// lazy on purpose: these objects refer back to the operators, so building them
// eagerly risks the initialisation cycle described at TOper's auxiliary constructor
object FOper {
  lazy val all = List(Equals, NotEquals, Less, LessEq, Divides)
}
object TOper {
  lazy val all = List(Plus,Times,Minus,Min,Max)
}

sealed abstract class ChainedFOper(s: String, r: HTMLRule, f: Int) extends FOper(s,r,f)

/* individual predicate symbols, function symbols, connectives, etc. */

/* uninterpreted operators, e.g., for FOL; these cannot be evaluated without using a model/
   see FOL/logic */
case class AbstractFunSym(name: String) extends TOper(name, Prefix(name), -1, None)
case class AbstractPredSym(name: String) extends FOper(name, Prefix(name), -1)

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
object Intersection extends TOper("intersect", Infix("∩"), 0)
object Union extends TOper("union", Infix("∪"), 0)
object SetDiff extends TOper("setdiff", Infix("∖"), -1, Some(2))
object Card extends TOper("card", FencedOperator("|","|"), -1, Some(1))
object MapsTo extends TOper("map", Infix("↦"), -1, Some(2))
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

/* concrete big operators */

object Sum extends BigOper("sum", "Σ")
object Product extends BigOper("prod", "Π")
object BigMax extends BigOper("max", "max")
object BigArgMax extends BigOper("argmax", "argmax")
object BigMin extends BigOper("min", "min")

/* other expressions */

object RangeSet extends TOper("range", null, -1) {
  override def toHTML(args: Seq[Expr]) =
    s"""<mrow><mo>{</mo>${args(0).toHTML}<mo>,</mo><mo>…</mo><mo>,</mo>${args(1).toHTML}<mo>}</mo></mrow>"""
}

/** probability */
case class Prob(of: Seq[Expr], conds: Seq[Expr]) extends Expr {
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

/** contexts/environments/assignments/records: assigns values to free variables, e.g. a=1, b=2
  *
  * `vals` lists the newest binding first. Two Contexts are equal if they bind the same
  * names to the same values, in whatever order.
  */
case class Context(vals: List[(String,Expr)]) extends Expr {
  def apply(n: String) = vals.find(_._1 == n).getOrElse(throw EvalError("undefined variable: " + n))._2
  def apply(v: (String,AnyVal)): Context = {
    Context((v._1, Expr(v._2))::vals)
  }
  def declares(n: String) = vals.exists(_._1 == n)

  def asMap: Map[String, Expr] = bindings.toMap

  /** one binding per name, sorted by name */
  def bindings: List[(String, Expr)] = vals.distinctBy(_._1).sortBy(_._1)

  override def equals(that: Any): Boolean = that match {
    case c: Context => asMap == c.asMap
    case _          => false
  }
  override def hashCode: Int = asMap.hashCode

  override def toString = bindings.map(v => v._1 + "=" + v._2.toString).mkString(", ")
  def toSTeX = bindings.map { case (n, v) => s"\\eq{$n,${v.toSTeX}}" }.mkString(",\\;")
  def toHTML = bindings.map { case (n, v) => s"<mrow>${Name.html(n)}<mo>=</mo>${v.toHTML}</mrow>" }
    .mkString("<mrow>", "<mo>,</mo>", "</mrow>")
  // the names are bound here, so only the values can contain free variables
  def fvs = bindings.flatMap(_._2.fvs)
}
object Context {
  def apply(): Context = Context(Nil)
  def apply(v: (String,Int)): Context = Context()(v)
}

/***************************************************************************/
/* Checking and Evaluation */

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

  def getLiteralDomains(l: Seq[Expr]) = l map {
    case l: Lit => l.domain
    case _ => throw EvalError("not a literal")
  }

  /** evaluates expressions to literals */
  def apply(exp: Expr)(implicit ctx: Context): Lit = exp match {
    case l: Lit => l
    case Var(n) => ctx(n) match {
      case l: Lit => l
      case e => apply(e)
    }
    case OperApply(op: ChainedFOper,as) => as match {
      case Nil => DBool(true)
      case hd :: tl =>
        var prev = apply(hd)
        val b = tl.forall {a =>
          val aE = apply(a)
          val r = op match {
            case Equals => prev == aE
            case Less => prev.asInt < aE.asInt
            case LessEq => prev.asInt <= aE.asInt
            case Divides => if (prev.asInt == 0) false else aE.asInt % prev.asInt == 0
          }
          prev = aE
          r
        }
        DBool(b)
    }
    case OperApply(op: TOper,as) =>
      val asE = as.map(a => apply(a))
      val doms = getLiteralDomains(asE)
      val allInt = doms.forall(_.isInstanceOf[IntegerDomain])
      if (allInt) {
        val fsE = asE.map(_.asInt)
        val r: Int = op match {
          case Plus => fsE.fold(0)((x: Int,y: Int) => x + y)
          case Times => fsE.fold(1)((x: Int,y: Int) => x * y)
          case Min => if (fsE.nonEmpty) fsE.reduce((x: Int,y: Int) => if (x > y) y else x) else 0
          case Max => if (fsE.nonEmpty) fsE.reduce((x: Int,y: Int) => if (x < y) y else x) else 0
          case Minus => fsE(0) - fsE(1)
          case Divide => fsE(0) / fsE(1)
          case Exp => exponential(fsE(0),fsE(1))
          case Mod =>
            val e = fsE(1)
            val m = fsE(0) % e
            if (m < 0) m + e else m
        }
        DInt(r)
      } else {
        val allNumeric = doms.forall(d => d == DDouble || d.isInstanceOf[IntegerDomain])
        if (allNumeric) {
          val vs = asE.map(_.asDouble)
          val d = op match {
            case Plus => vs.sum
            case Times => vs.product
            case Minus => vs(0) - vs(1)
            case Divide =>
              if (vs(1) == 0.0) throw EvalError("division by zero") else vs(0) / vs(1)
            case Exp => Math.pow(vs(0),vs(1))
            case Min => vs.min
            case Max => vs.max
            case _ => throw EvalError("cannot compute with " + op)
          }
          DDouble(d)
        } else {
          throw EvalError("not a number")
        }
      }
    case InSet(a :: e :: _) =>
      val aE = apply(a)
      val eE = apply(e)
      DBool(eE.value.asInstanceOf[List[?]].contains(aE))
    case NotEquals(as) =>
      val asE = as.map(a => apply(a))
      DBool(asE.length == asE.distinct.length)
    case Implies(as) =>
      val asE = as.map(a => apply(a).asBool)
      val b = asE match {
        case Nil => false
        case l => l.init.exists(a => !a) || l.last
      }
      DBool(b)
    case Neg(as) => DBool(as.exists(a => !apply(a).asBool))
    case And(as) => DBool(as.forall(a => apply(a).asBool))
    case Or(as) =>  DBool(as.exists(a => apply(a).asBool))
  }

  def exponential(a: Int,b: Int): Int = {
    if (b == 0) 1 else a * exponential(a,b - 1)
  }
}
