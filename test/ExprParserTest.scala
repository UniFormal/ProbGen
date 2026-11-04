package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B2: the parser for students' answers */
class ExprParserTest extends AnyFunSuite {
  import ExprParser.*

  private val p = BVar("p"); private val q = BVar("q"); private val r = BVar("r")
  private val a = Var("a"); private val b = Var("b"); private val c = Var("c")

  private def form(s: String): Form = parseForm(s).fold(e => fail(s"'$s': $e"), identity)
  private def term(s: String): Term = parseTerm(s).fold(e => fail(s"'$s': $e"), identity)

  // ── formulas ──────────────────────────────────────────────────────────────

  test("connectives in words, symbols and LaTeX all give the same tree") {
    val expected = Implies(And(p, Neg(q)), Or(q, r))
    for (s <- List(
      "p and not q -> q or r",
      "p ∧ ¬q ⇒ q ∨ r",
      "p && !q → q || r",
      "p & ~q implies q or r",
      "p \\wedge \\neg q \\Rightarrow q \\vee r",
      "P AND NEG q -> q OR r".replace("P ", "p ")
    )) assert(form(s) == expected, s)
  }

  test("precedence: not, and, or, -> (weakest)") {
    assert(form("not p and q") == And(Neg(p), q))
    assert(form("p or q and r") == Or(p, And(q, r)))
    assert(form("p -> q or r") == Implies(p, Or(q, r)))
    assert(form("p -> q -> r") == Implies(p, Implies(q, r))) // right-associative
    assert(form("(p or q) and r") == And(Or(p, q), r))
    assert(form("not (p and q)") == Neg(And(p, q)))
    assert(form("p and q and r") == And(p, q, r)) // one And with three arguments
  }

  test("true, false and relations") {
    assert(form("true or p") == Or(BLit(true), p))
    assert(form("x = 1") == Equals(Var("x"), DInt(1)))
    assert(form("x != 1") == NotEquals(Var("x"), DInt(1)))
    assert(form("x ≠ 1") == NotEquals(Var("x"), DInt(1)))
    assert(form("s in G") == InSet(Var("s"), Var("G")))
    assert(form("a = 1 -> b <= 3") == Implies(Equals(a, DInt(1)), LessEq(b, DInt(3))))
  }

  test("chained relations, and > >= stored as < <=") {
    assert(form("3 <= E <= 7") == LessEq(DInt(3), Var("E"), DInt(7)))
    assert(form("E > 4") == Less(DInt(4), Var("E")))
    assert(form("E >= 4") == LessEq(DInt(4), Var("E")))
    assert(form("E ≥ 4") == LessEq(DInt(4), Var("E")))
    assert(form("7 >= E >= 3") == LessEq(DInt(3), Var("E"), DInt(7)))
    assert(form("a < b <= c") == And(Less(a, b), LessEq(b, c)))
  }

  test("brackets around a term are not mistaken for a formula") {
    assert(form("(a + b) < c") == Less(Plus(a, b), c))
    assert(form("(a) = 1") == Equals(a, DInt(1)))
    assert(form("((p))") == p)
  }

  // ── terms ─────────────────────────────────────────────────────────────────

  test("numbers: integers, decimals, percent, negative") {
    assert(term("2") == DInt(2))
    assert(term("0.5") == DDouble(0.5))
    assert(term(".5") == DDouble(0.5))
    assert(term("-2") == DInt(-2))
    assert(term("60%") == Divide(DInt(60), DInt(100)))
    assert(Evaluator.real(term("60%")) == 0.6)
  }

  test("arithmetic: precedence and one node per sum/product") {
    assert(term("a + b + c") == Plus(a, b, c))
    assert(term("a * b * c") == Times(a, b, c))
    assert(term("a + b * c") == Plus(a, Times(b, c)))
    assert(term("a - b - c") == Minus(Minus(a, b), c))
    assert(term("a - b + c") == Plus(Minus(a, b), c))
    assert(term("(a + b) * c") == Times(Plus(a, b), c))
    assert(term("a / b / c") == Divide(Divide(a, b), c))
    assert(term("s ^ a + 3") == Plus(Exp(Var("s"), a), DInt(3)))
    assert(term("2 ^ 3 ^ 2") == Exp(DInt(2), Exp(DInt(3), DInt(2))))
    assert(term("(s^a + 3) mod 7") == Mod(Plus(Exp(Var("s"), a), DInt(3)), DInt(7)))
    assert(term("a + b mod c") == Plus(a, Mod(b, c)))
    assert(term("a · b") == Times(a, b))
    assert(term("a × b") == Times(a, b))
  }

  test("the values of typical answers") {
    assert(Evaluator.real(term("0.8 * 0.8")) == 0.8 * 0.8)
    assert(Evaluator.real(term("16/25")) == 0.64)
    assert(Evaluator.real(term("-2 + 0.5 * 4")) == 0.0)
  }

  test("names: primes, subscripts, Greek letters as LaTeX or Unicode") {
    assert(term("s'") == Var("s'"))
    assert(term("D_a") == Var("D_a"))
    assert(term("D_{a}") == Var("D_a"))
    assert(term("x_{10}") == Var("x_10"))
    assert(term("\\gamma") == Var("\\gamma"))
    assert(term("γ") == Var("\\gamma")) // the problems write it as \gamma
    assert(term("π") == Var("\\pi"))
  }

  test("function application, probabilities") {
    assert(term("U(s)") == FunApply(Var("U"), Var("s")))
    assert(term("T(s, a)") == FunApply(Var("T"), Var("s"), a))
    assert(term("b'(0)") == FunApply(Var("b'"), DInt(0)))
    assert(term("P(X = x)") == Prob(List(Equals(Var("X"), Var("x"))), Nil))
    assert(term("P(X = x | Y = y, Z = z)") ==
      Prob(List(Equals(Var("X"), Var("x"))), List(Equals(Var("Y"), Var("y")), Equals(Var("Z"), Var("z")))))
  }

  test("sets, tuples, sequences") {
    assert(term("{a, b}") == FinSet(a, b))
    assert(term("{}") == FinSet())
    assert(term("(a, b)") == Tuple(a, b))
    assert(term("⟨a, b⟩") == Tuple(a, b))
    assert(term("\\langle a, b \\rangle") == Tuple(a, b))
    assert(term("{(0,0), (0,1)}") == FinSet(Tuple(DInt(0), DInt(0)), Tuple(DInt(0), DInt(1))))
    assert(term("[a, b, a]") == FinSeq(a, b, a))
  }

  // ── equations and assignments ─────────────────────────────────────────────

  test("equations with any left side") {
    assert(parseEquations("b'(0) = 0.5, b'(1) = 0.25") == Right(List(
      FunApply(Var("b'"), DInt(0)) -> DDouble(0.5),
      FunApply(Var("b'"), DInt(1)) -> DDouble(0.25))))
    assert(parseEquations("S = {a, b}, A = {x}") == Right(List(Var("S") -> FinSet(a, b), Var("A") -> FinSet(Var("x")))))
  }

  test("one assignment, in any order and with truth values") {
    assert(parseContext("p = true, q = false") == Right(Context()("p" -> true)("q" -> false)))
    assert(parseContext("q=false,p=true") == Right(Context()("p" -> true)("q" -> false)))
    assert(parseContext("a=1, b=2, c=0, d=1") == Right(Context()("a" -> 1)("b" -> 2)("c" -> 0)("d" -> 1)))
    assert(parseContext("D_b = {0, 1}") == Right(Context(List("D_b" -> FinSet(DInt(0), DInt(1))))))
  }

  test("several assignments: ; or line breaks, and none") {
    val two = List(Context()("a" -> 1)("b" -> 2), Context()("a" -> 0)("b" -> 1))
    assert(parseContexts("a=1, b=2; a=0, b=1") == Right(two))
    assert(parseContexts("a=1, b=2\n a=0, b=1\n") == Right(two))
    assert(parseContexts("a=1, b=2;\n\na=0, b=1;") == Right(two))
    assert(parseContexts("none") == Right(Nil))
    assert(parseContexts("  ") == Right(Nil))
  }

  // ── errors are messages, not exceptions ───────────────────────────────────

  test("unreadable answers give a message") {
    def msg[A](r: Either[String, A]): String = r.fold(identity, x => fail(s"parsed as $x"))
    assert(msg(parseForm("p and")).contains("ends there"))
    assert(msg(parseForm("p q")).contains("found 'q'"))
    assert(msg(parseForm("1 + 2")).contains("Expected a formula"))
    assert(msg(parseTerm("(a, b")).contains("Expected ')'"))
    assert(msg(parseTerm("a @ b")).contains("'@'"))
    assert(msg(parseTerm("\\foo")).contains("Unknown command"))
    assert(msg(parseTerm("")).contains("enter an answer"))
    assert(msg(parseContext("a = 1, a = 2")).contains("more than one value"))
    assert(msg(parseContext("1 = a")).contains("must be a name"))
  }

  // ── round trips: everything the generators produce can be typed back in ──

  /** an expression in the syntax a student would type, fully bracketed */
  private def typed(e: Expr): String = e match {
    case BLit(v)              => v.toString
    case BVar(n)              => n
    case Var(n)               => n
    case Lit(v, _)            => v.toString
    case Conn(Neg, Seq(x))    => s"not (${typed(x)})"
    case Conn(op, args)       =>
      val word = op match { case And => "and"; case Or => "or"; case _ => "->" }
      args.map(x => s"(${typed(x)})").mkString(s" $word ")
    case Pred(op, Seq(x, y))  =>
      val sym = op match { case Equals => "="; case Less => "<"; case LessEq => "<="; case NotEquals => "!="; case _ => "in" }
      s"(${typed(x)}) $sym (${typed(y)})"
    case Apply(op, Seq(x, y)) =>
      val sym = op match { case Plus => "+"; case Minus => "-"; case Times => "*"; case Divide => "/"; case Exp => "^"; case _ => "mod" }
      s"(${typed(x)}) $sym (${typed(y)})"
    case other                => fail(s"no typed form for $other")
  }

  test("round trip: random Logic formulas") {
    for (_ <- 1 to 300) {
      val f = PropFormulaGenerator.generate(List("p", "q", "r"), 1, 4, 2)
      assert(parseForm(typed(f)) == Right(f), typed(f))
    }
  }

  test("round trip: random Search transition terms") {
    for (_ <- 1 to 100) {
      val t = SearchProblemGenerator.genTransition(8, Generator.chooseBoolean(0.5))
      assert(parseTerm(typed(t)) == Right(t), typed(t))
    }
  }
}
