package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** the Mathematical Notation problem: on random instances, the right answer typed in the
  * documented notation is graded correct, and typical wrong answers are not
  */
class NotationGradingTest extends AnyFunSuite {

  private def fresh(): NotationProblem = { val p = NotationProblemGenerator.make(); p.chooseSubproblems(); p }
  private def braces(xs: Iterable[Any]) = xs.mkString("{", ", ", "}")
  private val runs = 40

  test("every question renders as HTML and as TeX") {
    for (_ <- 1 to runs) {
      val p = fresh()
      val tex = p.toSTeX(p.chooseSubproblems()).toString
      assert(!tex.contains("???"))
      p.toSTeX(p.chooseSubproblems()).toHTML
    }
  }

  test("set operations: any order, braces, empty set") {
    for (_ <- 1 to runs) {
      val q = fresh().setOperation
      val r = q.op match {
        case Intersection => q.a intersect q.b
        case Union        => q.a union q.b
        case _            => q.a diff q.b
      }
      assert(q.check(braces(r.toList.sorted)) == Correct())
      assert(q.check(braces(r.toList.sorted.reverse)) == Correct())
      if (r.nonEmpty) assert(q.check("{}") != Correct())
      assert(q.check(braces((r + 42).toList)) != Correct())
    }
  }

  test("a function as a set of pairs") {
    for (_ <- 1 to runs) {
      val q = fresh().functionAsPairs
      val pairs = (0 to q.n).map(x => (x, Evaluator(q.body)(using Context("x" -> x)).asInt))
      assert(q.check(braces(pairs.map((a, b) => s"($a, $b)"))) == Correct())
      assert(q.check(pairs.reverse.map((a, b) => s"⟨$a,$b⟩").mkString(", ")) == Correct())
      assert(q.check(braces(pairs.map((a, b) => s"($b, $a)"))) != Correct()) // the wrong way round
      assert(q.check(braces(pairs.tail.map((a, b) => s"($a, $b)"))).isInstanceOf[PartiallyCorrect])
    }
  }

  test("Cartesian product") {
    for (_ <- 1 to runs) {
      val q = fresh().cartesianProduct
      val pairs = for (l <- q.left; r <- q.right) yield s"($l, $r)"
      assert(q.check(braces(pairs)) == Correct())
      assert(q.check(braces(pairs.reverse)) == Correct())
      assert(q.check(braces(for (l <- q.left; r <- q.right) yield s"($r, $l)")) != Correct())
    }
  }

  test("cardinality") {
    for (_ <- 1 to runs) {
      val q = fresh().cardinality
      val n = q.op match {
        case Union        => (q.a union q.b).size
        case Intersection => (q.a intersect q.b).size
        case _            => q.a.size * q.b.size
      }
      assert(q.check(n.toString) == Correct())
      assert(q.check((n + 1).toString).isInstanceOf[Incorrect])
    }
  }

  test("truth values: yes/no, and the formula itself is a well-formed Boolean") {
    for (_ <- 1 to runs) {
      val q = fresh().truthValue
      assert(q.check(if (q.truth) "yes" else "no") == Correct())
      assert(q.check(if (q.truth) "no" else "yes").isInstanceOf[Incorrect])
      // the question's formula, printed and read back, is the same expression
      assert(ExprParser.parseExpr(ExprCompare.show(q.formula)).isRight, ExprCompare.show(q.formula))
    }
  }

  test("arithmetic: the value, not the expression") {
    for (_ <- 1 to runs) {
      val q = fresh().arithmetic
      val v = Evaluator(q.term)(using Context()).asInt
      assert(q.check(v.toString) == Correct())
      assert(q.check((v + 1).toString).isInstanceOf[Incorrect])
      assert(q.check("(1 + 2) * 3 mod 4").isInstanceOf[MalformedInput])
      assert(q.check(s"$v + 0").isInstanceOf[MalformedInput])
    }
  }
}
