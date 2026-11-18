package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B6 (CSP): on random problems, the model answer typed in is graded correct,
  * and typical wrong answers are not
  */
class CSPGradingTest extends AnyFunSuite {

  /** a fresh random CSP problem with its questions initialised (like on the page) */
  private def fresh(): CSPProblem = { val p = CSPGenerator.make(); p.chooseSubproblems(); p }
  private def typed(c: Context) = ExprCompare.show(c)
  private val runs = 25

  test("all solutions: in any order, partial credit for some, wrong for none") {
    for (_ <- 1 to runs) {
      val p = fresh()
      val sols = p.csp.solve()
      val q = p.giveSolutions
      assert(q.check(sols.map(typed).mkString("; ")) == Correct())
      assert(q.check(sols.reverse.map(typed).mkString("\n")) == Correct())
      if (sols.length > 1) assert(q.check(typed(sols.head)).isInstanceOf[PartiallyCorrect])
      assert(q.check("none").isInstanceOf[Incorrect])
      assert(q.check("a=1").isInstanceOf[MalformedInput]) // not a total assignment
    }
  }

  test("an inconsistent assignment: any one is right, a solution is wrong") {
    for (_ <- 1 to runs) {
      val p = fresh()
      val q = p.giveInconsistentAssignment
      assert(q.check(typed(p.csp.findInconsistentAssignment().get)) == Correct())
      assert(q.check(typed(p.csp.solve().head)).isInstanceOf[Incorrect])
      assert(q.check("a=1, b=0, c=0").isInstanceOf[MalformedInput])        // d is missing
      assert(q.check("a=1, b=0, c=0, d=99").isInstanceOf[MalformedInput])  // 99 is not in D_d
    }
  }

  test("arc consistency: T/F per box in order") {
    for (_ <- 1 to runs) {
      val p = fresh()
      val q = p.checkArcConsistency
      val truth = q.testPairs.map { case (v, w) => p.csp.isArcConsistent(v, w) }
      assert(q.check(truth.map(b => if (b) "T" else "F").mkString(", ")) == Correct())
      assert(q.check(truth.map(b => if (b) "F" else "T").mkString(", ")).isInstanceOf[Incorrect])
      assert(q.check("T, F").isInstanceOf[MalformedInput]) // 4 boxes, 2 answers
    }
  }

  test("forward checking: the new domains, credit per variable") {
    for (_ <- 1 to runs) {
      val p = fresh()
      val q = p.forwardChecking
      val doms = p.csp.forwardChecking(p.assignedVar, p.assignedVal)
      val rest = p.csp.variables.filter(_ != p.assignedVar)
      def answer(d: Map[String, List[Int]]) = rest.map(v => s"D_$v = {${d(v).mkString(", ")}}").mkString(", ")
      assert(q.check(answer(doms)) == Correct())
      assert(q.check(answer(doms).replace("D_", "D_{").replaceAll("( =)", "}$1")) == Correct()) // D_{b} works too
      // the unchanged domains are not all right (forward checking removes something)
      if (rest.exists(v => doms(v) != p.csp.domains(v)))
        assert(q.check(answer(p.csp.domains)) != Correct())
    }
  }
}
