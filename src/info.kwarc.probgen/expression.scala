package info.kwarc.probgen

abstract class Domain { def apply(v: Any) = Lit(v, this) }
abstract class IntegerDomain extends Domain
trait OrderedDomain { def values: List[Any] }
case class DUpto(n: Int) extends IntegerDomain with OrderedDomain {
  def values = Range(0, n).toList
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
  def toText: String // clean plain text for answer comparison
}

trait ExprLike { def toExpr: Expr }

case class Context(vals: List[(String, Any)]) {
  def apply(n: String) = vals
    .find(_._1 == n)
    .getOrElse(throw EvalError("undefined variable: " + n))
    ._2
  def apply(v: (String, AnyVal)): Context = Context(v :: vals)
}
object Context {
  def apply(v: (String, Int)): Context = Context(List(v))
}

implicit class AnyToExpr(a: Any) { def unary_! = Expr(a) }

object Expr {
  implicit def fromInt(i: Int): Term = DInt(i)
  implicit def fromDobule(d: Double): Term = DDouble(d)
  implicit def stringToId(s: String): Var = Var(s)
  def !(a: Any) = apply(a)
  def apply(a: Any) = fromAny(a)
  def fromAnyO(a: Any): Option[Expr] =
    try { Some(fromAny(a)) }
    catch { case _: Exception => None }
  def fromAny(a: Any): Term = a match {
    case e: ExprLike     => fromAny(e.toExpr)
    case e: Term         => e
    case i: Int          => DInt(i)
    case s: String       => DString(s)
    case l: Seq[_]       => FinSeq(l.map(fromAny)*)
    case s: Set[_]       => FinSet(s.toList.map(fromAny)*)
    case t: Tuple2[_, _] => Tuple(t.productIterator.toList.map(fromAny)*)
  }
}

// ── Formulas ──────────────────────────────────────────────────────────────

sealed abstract class Form extends Expr
case class Conn(op: COper, args: Seq[Form]) extends Form {
  // Every child that is itself a connective application gets parenthesized,
  // full stop - not just where operator-precedence would technically
  // require it (FormulaParser's grammar implies Implies binds loosest, then
  // Or, then And, then Neg tightest, with Implies right-associative).
  // Relying on a reader to know that table to correctly read an unbracketed
  // formula makes it genuinely unclear; always bracketing every nested
  // connective removes that ambiguity entirely, at the cost of a few more
  // parens than strictly necessary. Bare variables/predicates are left
  // unparenthesized, and the outermost formula isn't wrapped since it has
  // no parent to disambiguate from.
  private def rendered(render: Form => String): Seq[String] =
    args.map {
      case c: Conn => s"(${render(c)})"
      case a       => render(a)
    }

  def toSTeX = op.sTeX(rendered(_.toSTeX))
  def toHTML = op.html(rendered(_.toHTML))
  def toText = op.text(rendered(_.toText))
}
case class Pred(op: FOper, args: Seq[Term]) extends Form {
  def toSTeX = op.sTeX(args.map(_.toSTeX))
  def toHTML = op.html(args.map(_.toHTML))
  def toText = op.text(args.map(_.toText))
  def ===(arg: Term) = op match {
    case op: ChainedFOper => Pred(op, args :+ arg)
    case _                => throw EvalError("not a chained operator")
  }
}
case class BVar(name: String) extends Form {
  def toSTeX = name
  def toHTML = s"<i>$name</i>"
  def toText = name
}

// ── Terms ──────────────────────────────────────────────────────────────────

abstract class Term extends Expr {
  def ===(t: Term) = Equals(this, t)
  def =!=(t: Term) = NotEquals(this, t)
  def apply(args: Term*) = FunApply(this +: args*)
  def +(arg: Term) = Plus(this, arg)
  def *(arg: Term) = Times(this, arg)
  def -(arg: Term) = Minus(this, arg)
  def /(arg: Term) = Divide(this, arg)
  infix def in(arg: Term) = InSet(this, arg)
  infix def to(arg: Term) = RangeSet(this, arg)
}

case class Apply(op: TOper, args: Seq[Term]) extends Term {
  def toSTeX = op.sTeX(args.map(_.toSTeX))
  def toHTML = op.html(args.map(_.toHTML))
  def toText = op.text(args.map(_.toText))
}

case class BigApply(op: BigOper, conds: Seq[Form], body: Term) extends Term {
  def toSTeX =
    s"\\${op.stexname}_{${conds.map(_.toSTeX).mkString(",\\,")}}{${body.toSTeX}}"
  def toHTML = {
    val cs = conds.map(_.toHTML).mkString(", ")
    s"${op.sym}<sub>$cs</sub>(${body.toHTML})"
  }
  def toText =
    s"${op.sym}(${conds.map(_.toText).mkString(",")})(${body.toText})"
}

case class Var(name: String) extends Term {
  def toSTeX = name
  def toHTML = name match {
    case "\\gamma"                                         => "<i>γ</i>"
    case "\\pi"                                            => "<i>π</i>"
    case "\\sigma"                                         => "<i>σ</i>"
    case "\\mu"                                            => "<i>μ</i>"
    case "\\alpha"                                         => "<i>α</i>"
    case "\\beta"                                          => "<i>β</i>"
    case "\\theta"                                         => "<i>θ</i>"
    case "\\lambda"                                        => "<i>λ</i>"
    case "\\to"                                            => "→"
    case "\\times"                                         => "×"
    case s if s.startsWith("\\mathtt{") && s.endsWith("}") =>
      s"<span class='mathtt'>${s.stripPrefix("\\mathtt{").stripSuffix("}")}</span>"
    case s if s.startsWith("\\mathrm{") && s.endsWith("}") =>
      s.stripPrefix("\\mathrm{").stripSuffix("}")
    case s => s"<i>$s</i>"
  }
  def toText = name match {
    case "\\gamma"                                         => "γ"
    case "\\pi"                                            => "π"
    case "\\sigma"                                         => "σ"
    case "\\mu"                                            => "μ"
    case "\\alpha"                                         => "α"
    case "\\beta"                                          => "β"
    case "\\theta"                                         => "θ"
    case "\\lambda"                                        => "λ"
    case "\\to"                                            => "→"
    case "\\times"                                         => "×"
    case s if s.startsWith("\\mathtt{") && s.endsWith("}") =>
      s.stripPrefix("\\mathtt{").stripSuffix("}")
    case s if s.startsWith("\\mathrm{") && s.endsWith("}") =>
      s.stripPrefix("\\mathrm{").stripSuffix("}")
    case s => s
  }
}

case class Lit(value: Any, domain: Domain) extends Term {
  def toSTeX = value.toString
  def toHTML: String = value match {
    // Handle double-wrapped case: Lit(Lit("b",DString), DString)
    case inner: Lit => inner.toHTML
    case _          =>
      domain match {
        case DString =>
          val v = value.toString
          v match {
            case s if s.startsWith("\\mathtt{") && s.endsWith("}") =>
              s"<span class='mathtt'>${s.stripPrefix("\\mathtt{").stripSuffix("}")}</span>"
            case "\\to"    => "→"
            case "\\gamma" => "<i>γ</i>"
            case s         => s"<i>$s</i>"
          }
        case _ => s"<span class='num'>${value.toString}</span>"
      }
  }
  def toText: String = value match {
    // Handle double-wrapped case: Lit(Lit("b",DString), DString)
    case inner: Lit => inner.toText
    case _          =>
      domain match {
        case DString =>
          val v = value.toString
          v match {
            case s if s.startsWith("\\mathtt{") && s.endsWith("}") =>
              s.stripPrefix("\\mathtt{").stripSuffix("}")
            case "\\to"    => "→"
            case "\\gamma" => "γ"
            case s         => s
          }
        case _ => value.toString
      }
  }
  def asInt = if (domain == DInt) value.asInstanceOf[Int]
  else throw EvalError("value not an integer: " + this)
}

object NameLit {
  def apply(i: Int): Lit = DString((97 + i).toChar.toString)
  def applyUpper(i: Int): Lit = DString((65 + i).toChar.toString)
}

case class Prob(of: Seq[Expr], conds: Seq[Expr]) extends Term {
  def toSTeX = {
    val ofS = of.map(_.toSTeX).mkString(",\\,")
    val cS = conds.map(_.toSTeX).mkString(",\\,")
    val (nm, as) =
      if (conds.isEmpty) ("uProb", Seq(SPlainText(ofS)))
      else ("CondProb", Seq(SPlainText(ofS), SPlainText(cS)))
    SMacroApplication(nm, as, false).toString
  }
  def toHTML = {
    val ofH = of.map(_.toHTML).mkString(", ")
    if (conds.isEmpty) s"<i>P</i>($ofH)"
    else s"<i>P</i>($ofH | ${conds.map(_.toHTML).mkString(", ")})"
  }
  def toText = {
    val ofT = of.map(_.toText).mkString(", ")
    if (conds.isEmpty) s"P($ofT)"
    else s"P($ofT | ${conds.map(_.toText).mkString(", ")})"
  }
}

// ── Operator base ──────────────────────────────────────────────────────────

sealed abstract class Oper {
  def stexname: String
  def flexary: Boolean
  def sTeX(args: Seq[String]): String
  def html(args: Seq[String]): String
  def text(args: Seq[String]): String // plain text rendering
}

sealed abstract class BigOper(val stexname: String, val sym: String) {
  def apply(conds: Form*)(body: Term) = BigApply(this, conds, body)
}

sealed abstract class FOper(val stexname: String, val flexary: Boolean)
    extends Oper {
  def apply(args: Term*) = Pred(this, args.toList)
  def unapply(f: Form) = f match {
    case Pred(op, as) if op == this => Some(as); case _ => None
  }
}
sealed abstract class TOper(
    val stexname: String,
    val flexary: Boolean,
    val arity: Option[Int] = None
) extends Oper {
  def apply(args: Term*): Term = Apply(this, args.toList)
  def apply(args: List[Int]): Term = apply(args.map(DInt.apply)*)
  def unapply(f: Term) = f match {
    case Apply(op, as) if op == this => Some(as); case _ => None
  }
  def minArity = arity; def maxArity = arity
}
sealed abstract class COper(val stexname: String, val flexary: Boolean)
    extends Oper {
  def apply(args: Form*) = Conn(this, args.toList)
  def unapply(f: Form) = f match {
    case Conn(op, as) if op == this => Some(as); case _ => None
  }
}
sealed abstract class ChainedFOper(s: String, f: Boolean) extends FOper(s, f)

object FOper { val all = List(Equals, NotEquals, Less, LessEq, Divides) }
object TOper { val all = List(Plus, Times, Minus, Min, Max) }

// ── Connectives ────────────────────────────────────────────────────────────
object And extends COper("lconj", true) {
  def sTeX(a: Seq[String]) = a.mkString(" \\wedge ")
  def html(a: Seq[String]) = a.mkString(" ∧ ")
  def text(a: Seq[String]) = a.mkString(" and ")
}
object Or extends COper("ldisj", true) {
  def sTeX(a: Seq[String]) = a.mkString(" \\vee ")
  def html(a: Seq[String]) = a.mkString(" ∨ ")
  def text(a: Seq[String]) = a.mkString(" or ")
}
object Implies extends COper("implies", true) {
  def sTeX(a: Seq[String]) = a.mkString(" \\Rightarrow ")
  def html(a: Seq[String]) = a.mkString(" ⇒ ")
  def text(a: Seq[String]) = a.mkString(" => ")
}
object Neg extends COper("lneg", true) {
  def sTeX(a: Seq[String]) = "\\neg " + a.mkString("")
  def html(a: Seq[String]) = "¬" + a.mkString("")
  def text(a: Seq[String]) = "not " + a.mkString("")
}

// ── Predicate symbols ──────────────────────────────────────────────────────
object Equals extends ChainedFOper("equals", false) {
  def sTeX(a: Seq[String]) = a.mkString(" = ")
  def html(a: Seq[String]) = a.mkString(" = ")
  def text(a: Seq[String]) = a.mkString(" = ")
}
object NotEquals extends FOper("nequals", false) {
  def sTeX(a: Seq[String]) = a.mkString(" \\neq ")
  def html(a: Seq[String]) = a.mkString(" ≠ ")
  def text(a: Seq[String]) = a.mkString(" != ")
}
object Less extends ChainedFOper("intlessthan", false) {
  def sTeX(a: Seq[String]) = a.mkString(" < ")
  def html(a: Seq[String]) = a.mkString(" &lt; ")
  def text(a: Seq[String]) = a.mkString(" < ")
}
object LessEq extends ChainedFOper("intlethan", false) {
  def sTeX(a: Seq[String]) = a.mkString(" \\leq ")
  def html(a: Seq[String]) = a.mkString(" ≤ ")
  def text(a: Seq[String]) = a.mkString(" <= ")
}
object Divides extends ChainedFOper("intdivisible", false) {
  def sTeX(a: Seq[String]) = a.mkString(" | ")
  def html(a: Seq[String]) = a.mkString(" | ")
  def text(a: Seq[String]) = a.mkString(" | ")
}
object InSet extends FOper("inset", false) {
  def sTeX(a: Seq[String]) = a.mkString(" \\in ")
  def html(a: Seq[String]) = a.mkString(" ∈ ")
  def text(a: Seq[String]) = a.mkString(" in ")
}

// ── Function symbols ───────────────────────────────────────────────────────
object Plus extends TOper("intplus", true) {
  def sTeX(a: Seq[String]) = a.mkString(" + ")
  def html(a: Seq[String]) = a.mkString(" + ")
  def text(a: Seq[String]) = a.mkString(" + ")
}
object Minus extends TOper("intminus", true, Some(2)) {
  def sTeX(a: Seq[String]) = a.mkString(" - ")
  def html(a: Seq[String]) = a.mkString(" − ")
  def text(a: Seq[String]) = a.mkString(" - ")
}
object Times extends TOper("inttimes", true) {
  def sTeX(a: Seq[String]) = a.mkString(" \\cdot ")
  def html(a: Seq[String]) = a.mkString(" · ")
  def text(a: Seq[String]) = a.mkString(" * ")
}
object Divide extends TOper("realdivide", false) {
  def sTeX(a: Seq[String]) = a.mkString(" / ")
  def html(a: Seq[String]) =
    if (a.length >= 2)
      s"<span class='frac'><span>${a(0)}</span><span>${a(1)}</span></span>"
    else a.mkString(" / ")
  def text(a: Seq[String]) = a.mkString(" / ")
}
object Exp extends TOper("intpower", false, Some(2)) {
  def sTeX(a: Seq[String]) = s"${a(0)}^{${a(1)}}"
  def html(a: Seq[String]) = s"${a(0)}<sup>${a(1)}</sup>"
  def text(a: Seq[String]) = s"${a(0)}^${a(1)}"
}
object Mod extends TOper("intmod", false, Some(2)) {
  def sTeX(a: Seq[String]) = s"${a(0)} \\bmod ${a(1)}"
  def html(a: Seq[String]) = s"${a(0)} mod ${a(1)}"
  def text(a: Seq[String]) = s"${a(0)} mod ${a(1)}"
}
object Min extends TOper("intmin", true) {
  override def minArity = Some(2)
  def sTeX(a: Seq[String]) = s"\\min(${a.mkString(", ")})"
  def html(a: Seq[String]) = s"min(${a.mkString(", ")})"
  def text(a: Seq[String]) = s"min(${a.mkString(", ")})"
}
object Max extends TOper("intmax", true) {
  override def minArity = Some(2)
  def sTeX(a: Seq[String]) = s"\\max(${a.mkString(", ")})"
  def html(a: Seq[String]) = s"max(${a.mkString(", ")})"
  def text(a: Seq[String]) = s"max(${a.mkString(", ")})"
}
object FunApply extends TOper("apply", true) {
  def sTeX(a: Seq[String]) = s"${a.head}(${a.tail.mkString(", ")})"
  def html(a: Seq[String]) = s"${a.head}(${a.tail.mkString(", ")})"
  def text(a: Seq[String]) = s"${a.head}(${a.tail.mkString(", ")})"
}
object FinSet extends TOper("set", true) {
  def sTeX(a: Seq[String]) = s"\\{${a.mkString(", ")}\\}"
  def html(a: Seq[String]) = s"{${a.mkString(", ")}}"
  def text(a: Seq[String]) = s"{${a.mkString(", ")}}"
}
object RangeSet extends TOper("range", false) {
  def sTeX(a: Seq[String]) = s"\\range{${a(0)}}{${a(1)}}"
  def html(a: Seq[String]) = s"{${a(0)}, …, ${a(1)}}"
  def text(a: Seq[String]) = s"{${a(0)}, ..., ${a(1)}}"
}
object Tuple extends TOper("tup", true) {
  def sTeX(a: Seq[String]) = s"(${a.mkString(", ")})"
  def html(a: Seq[String]) = s"(${a.mkString(", ")})"
  def text(a: Seq[String]) = s"(${a.mkString(", ")})"
}
object FinSeq extends TOper("seq", true) {
  def sTeX(a: Seq[String]) = a.mkString(", ")
  def html(a: Seq[String]) = a.mkString(", ")
  def text(a: Seq[String]) = a.mkString(", ")
}
object TransitionChain extends TOper("transitions", true) {
  def sTeX(a: Seq[String]) = a.mkString(" → ")
  def html(a: Seq[String]) = {
    val steps = a.tail
      .grouped(2)
      .map {
        case Seq(act, st) => s" →<sub>$act</sub> $st"
        case Seq(st)      => s" → $st"
        case _            => ""
      }
      .mkString("")
    a.head + steps
  }
  def text(a: Seq[String]) = a.mkString(" -> ")
}

object Sum extends BigOper("sum", "Σ")
object Product extends BigOper("times", "Π")
object BigMax extends BigOper("max", "max")
object BigArgMax extends BigOper("argmax", "argmax")
object BigMin extends BigOper("min", "min")

// ── Evaluator ──────────────────────────────────────────────────────────────

case class EvalError(m: String) extends Exception(m)

object Evaluator {
  def apply(f: Form)(implicit ctx: Context): Boolean = f match {
    case BVar(n) =>
      ctx(n) match {
        case v: Boolean => v
        case v          => throw EvalError("not Boolean: " + n + "=" + v)
      }
    case Pred(op: ChainedFOper, as) =>
      as match {
        case Nil      => true
        case hd :: tl =>
          var prev = apply(hd).asInt
          tl.forall { a =>
            val aE = apply(a).asInt
            val r = op match {
              case Equals  => prev == aE
              case Less    => prev < aE
              case LessEq  => prev <= aE
              case Divides => if (prev == 0) false else aE % prev == 0
            }
            prev = aE; r
          }
      }
    case InSet(a :: e :: _) =>
      val v = apply(a).asInt
      apply(e).value.asInstanceOf[List[Int]].contains(v)
    case NotEquals(as) => as.map(apply).distinct.length == as.length
    case Implies(as)   =>
      val ev = as.map(apply)
      ev match { case Nil => false; case l => l.init.exists(!_) || l.last }
    case Neg(as) => as.exists(a => !apply(a))
    case And(as) => as.forall(apply)
    case Or(as)  => as.exists(apply)
  }

  def apply(t: Term)(implicit ctx: Context): Lit = t match {
    case l: Lit => l
    case Var(n) =>
      ctx(n) match {
        case v: Int => DInt(v)
        case l: Lit => l
        case v      => throw EvalError("not integer: " + n + "=" + v)
      }
    case Apply(op, fs) =>
      val fsE = fs.map(a => apply(a).asInt)
      DInt(op match {
        case Plus  => fsE.fold(0)(_ + _)
        case Times => fsE.fold(1)(_ * _)
        case Min   =>
          if (fsE.nonEmpty) fsE.reduce((x, y) => if (x > y) y else x) else 0
        case Max =>
          if (fsE.nonEmpty) fsE.reduce((x, y) => if (x < y) y else x) else 0
        case Minus  => fsE(0) - fsE(1)
        case Divide => fsE(0) / fsE(1)
        case Exp    => exp(fsE(0), fsE(1))
        case Mod => val e = fsE(1); val m = fsE(0) % e; if (m < 0) m + e else m
      })
  }
  def exp(a: Int, b: Int): Int = if (b == 0) 1 else a * exp(a, b - 1)
}
