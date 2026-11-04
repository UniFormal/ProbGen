package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B3: graders that read answers with ExprParser and compare values with ExprCompare */
class GraderTest extends AnyFunSuite {

  private def correct(r: CheckResult)  = assert(r == Correct(), r)
  private def wrong(r: CheckResult)    = assert(r.isInstanceOf[Incorrect], r)
  private def unreadable(r: CheckResult) = assert(r.isInstanceOf[MalformedInput], r)
  private def partly(r: CheckResult, fraction: Double) = r match {
    case PartiallyCorrect(f, _) => assert(Math.abs(f - fraction) < 1e-9, r)
    case other                  => fail(s"expected partial credit $fraction, got $other")
  }
  private def hint(r: CheckResult): String = r match {
    case PartiallyCorrect(_, h) => h; case Incorrect(h) => h; case MalformedInput(h) => h; case o => o.toString
  }

  // ── comparing values ──────────────────────────────────────────────────────

  test("ExprCompare: the same value written differently") {
    import ExprCompare.sameValue
    assert(sameValue(DDouble(0.5), Divide(DInt(1), DInt(2))))
    assert(sameValue(DInt(1), DDouble(1.0)))
    assert(sameValue(DString("\\mathtt{up}"), Var("up")))
    assert(sameValue(DString("-1"), DInt(-1)))       // circular MDP actions are strings
    assert(sameValue(Var("-2"), DInt(-2)))           // MDP writes R(s) === "-2"
    assert(sameValue(FinSet(Var("a"), Var("b")), FinSet(Var("b"), Var("a"), Var("a"))))
    assert(sameValue(Tuple(DInt(0), DInt(1)), Tuple(DDouble(0.0), DDouble(1.0))))
    assert(!sameValue(Tuple(DInt(0), DInt(1)), Tuple(DInt(1), DInt(0))))
    assert(!sameValue(Var("a"), Var("b")))
    assert(sameValue(DDouble(0.333), Divide(DInt(1), DInt(3)), tol = 1e-3))
  }

  test("ExprCompare.show writes values the way a student types them") {
    import ExprCompare.show
    assert(show(FinSet(Tuple(DInt(0), DInt(1)), Var("a"))) == "{(0, 1), a}")
    assert(show(DString("\\mathtt{up}")) == "up")
    assert(show(DDouble(0.5)) == "0.5")
    assert(show(Context()("b" -> 2)("a" -> 1)) == "a=1, b=2")
    assert(show(FunApply(Var("b'"), DInt(0))) == "b'(0)")
    assert(show(Plus(Var("a"), Times(Var("b"), Var("c")))) == "a + (b * c)")
  }

  // ── numbers ───────────────────────────────────────────────────────────────

  test("NumberGrader: any way of writing the number") {
    val g = NumberGrader(0.64)
    for (a <- List("0.64", ".64", "16/25", "64%", "0.8*0.8", "0.8 · 0.8", "P = 0.8*0.8 = 0.64", "P(x) = 0.64"))
      correct(g.grade(a))
    assert(hint(g.grade("0.7")).contains("too large"))
    assert(hint(g.grade("0.5")).contains("too small"))
    unreadable(g.grade("a + b"))
    unreadable(g.grade("0.8 *"))
    correct(NumberGrader(-2).grade("-2"))
  }

  // ── sums and quotients of event names (Probabilities) ─────────────────────

  test("TermSumGrader: order free, partial credit, typos and repetitions") {
    val g = TermSumGrader(Set("a", "c", "f"), Set("a", "b", "c", "d", "e", "f"))
    correct(g.grade("a + c + f"))
    correct(g.grade("f+a+c"))
    correct(g.grade("(a + c) + f"))
    partly(g.grade("a + c"), 2.0 / 3)
    partly(g.grade("a + c + f + b"), 3.0 / 4)
    assert(hint(g.grade("a + a + c + f")).contains("more than once"))
    unreadable(g.grade("a + z"))           // z is not an event of this problem
    unreadable(g.grade("a + 2"))           // 2 is not a name
    wrong(g.grade("b + d"))
  }

  test("RatioGrader: numerator and denominator graded separately") {
    val g = RatioGrader(Set("a", "c"), Set("a", "b", "c"), Set("a", "b", "c", "d"))
    correct(g.grade("(a+c)/(a+b+c)"))
    correct(g.grade("(c + a) / (b + c + a)"))
    partly(g.grade("(a+c)/(a+b)"), (1.0 + 2.0 / 3) / 2)
    unreadable(g.grade("a+c / a+b+c"))     // without brackets this is not a quotient of sums
    unreadable(g.grade("a + c"))
  }

  // ── sets of values ────────────────────────────────────────────────────────

  test("ExprSetGrader: braces optional, order free, values compared by meaning") {
    val g = ExprSetGrader(List(Var("F"), Var("G"), Var("I")), "node")
    correct(g.grade("{F, G, I}"))
    correct(g.grade("I, G, F"))
    partly(g.grade("F, G"), 2.0 / 3)
    partly(g.grade("F, G, I, J"), 3.0 / 4)
    assert(hint(g.grade("F, G, I, J")).contains("J"))     // extra items are named ...
    assert(!hint(g.grade("F, G")).contains("I"))          // ... missing ones only counted
    wrong(g.grade("{}"))

    val actions = ExprSetGrader(List(DString("\\mathtt{up}"), DString("\\mathtt{down}")))
    correct(actions.grade("{up, down}"))

    val pairs = ExprSetGrader(List(Tuple(DInt(0), DInt(0)), Tuple(DInt(1), DInt(0))))
    correct(pairs.grade("(1,0), (0,0)"))
    correct(pairs.grade("{⟨0,0⟩, ⟨1,0⟩}"))
    unreadable(pairs.grade("(1,0"))
  }

  // ── sets of assignments ───────────────────────────────────────────────────

  test("ContextSetGrader: any order of assignments and of names") {
    val sols = List(Context()("a" -> 1)("b" -> 2), Context()("a" -> 0)("b" -> 1))
    val g = ContextSetGrader(sols, Set("a", "b"))
    correct(g.grade("a=1, b=2; a=0, b=1"))
    correct(g.grade("b=1, a=0\na=1, b=2"))
    partly(g.grade("a=1, b=2"), 1.0 / 2)
    partly(g.grade("a=1, b=2; a=0, b=1; a=3, b=3"), 2.0 / 3)
    wrong(g.grade("none"))
    unreadable(g.grade("a=1"))                 // b is missing: a format problem, not a wrong answer
    unreadable(g.grade("a=1, b=2, c=3"))
    assert(hint(g.grade("a=1")).contains("exactly"))
  }

  test("ContextSetGrader: truth values, and 'none' when nothing satisfies") {
    val one = ContextSetGrader(List(Context()("p" -> true)("q" -> false)), Set("p", "q"))
    correct(one.grade("q = false, p = true"))
    wrong(one.grade("p = false, q = true"))
    correct(ContextSetGrader(Nil, Set("p")).grade("none"))
  }

  // ── labelled values ───────────────────────────────────────────────────────

  test("EquationsGrader: labelled values, tolerance, omitted zeros") {
    val b = Var("b'")
    val g = EquationsGrader(List(b(DInt(0)) -> DDouble(0.75), b(DInt(1)) -> DDouble(0.25), b(DInt(2)) -> DInt(0)),
      omitZeros = true)
    correct(g.grade("b'(0) = 0.75, b'(1) = 0.25"))
    correct(g.grade("b'(1) = 1/4, b'(0) = 3/4, b'(2) = 0"))
    correct(g.grade("b'(0) = 0.7500001, b'(1) = 0.25"))
    partly(g.grade("b'(0) = 0.75, b'(1) = 0.3"), 1.0 / 2)
    partly(g.grade("b'(0) = 0.75, b'(1) = 0.25, b'(3) = 0.1"), 2.0 / 3)
    assert(hint(g.grade("b'(0) = 0.75, b'(1) = 0.25, b'(3) = 0.1")).contains("b'(3)"))
    unreadable(g.grade("b'(0) = 0.75, b'(0) = 0.25"))
    unreadable(g.grade("0.75, 0.25"))
  }

  test("EquationsGrader: grid states as labels") {
    val g = EquationsGrader(List(FunApply(Var("b'"), Tuple(DInt(0), DInt(1))) -> DDouble(0.5)))
    correct(g.grade("b'((0,1)) = 0.5"))
    correct(g.grade("b'(⟨0,1⟩) = 1/2"))
  }
}
