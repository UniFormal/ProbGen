package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B6 (Logic): on random problems, model answers typed in are graded correct,
  * and typical wrong answers are not
  */
class LogicGradingTest extends AnyFunSuite {

  private def fresh(): PropLogicProblem = { val p = LogicProblemGenerator.make(); p.chooseSubproblems(); p }
  private def typed(e: Expr) = ExprCompare.show(e)
  private val runs = 40
  private val p = Var("p"); private val q = Var("q")

  test("CNF and DNF: any equivalent formula of the right shape") {
    for (_ <- 1 to runs) {
      val pr = fresh()
      val cnf = PropLogic.toCNF(pr.formula)
      val dnf = PropLogic.toDNF(pr.formula)
      assert(pr.ConvertToCNF.check(typed(cnf)) == Correct(), typed(cnf))
      assert(pr.ConvertToDNF.check(typed(dnf)) == Correct(), typed(dnf))
      // the same answers in symbols instead of words
      val symbols = typed(cnf).replace(" and ", " ∧ ").replace(" or ", " ∨ ").replace("not ", "¬")
      assert(pr.ConvertToCNF.check(symbols) == Correct(), symbols)
      // the original formula is equivalent, but (usually) not in CNF
      if (!PropLogic.isCNF(pr.formula))
        assert(hint(pr.ConvertToCNF.check(typed(pr.formula))).contains("not in CNF"))
    }
    assert(hint(fresh().ConvertToCNF.check("p and")).nonEmpty) // unreadable, not a crash
  }

  test("CNF: right shape but not equivalent") {
    val pr = PropLogicProblem(Or(p, q)); pr.chooseSubproblems()
    assert(hint(pr.ConvertToCNF.check("p and q")).contains("not equivalent"))
    assert(pr.ConvertToCNF.check("q or p") == Correct())
  }

  test("satisfying / falsifying assignments: any order, partial credit") {
    for (_ <- 1 to runs) {
      val pr = fresh()
      for ((q, satisfying) <- List(pr.GiveSatisfyingAssignments -> true, pr.GiveFalsifyingAssignments -> false)) {
        val all = PropLogic.findAllAssignments(q.ownFormula, satisfying).toList
        val answer = if (all.isEmpty) "none" else all.map(typed).mkString("; ")
        assert(q.check(answer) == Correct(), answer)
        assert(q.check(all.reverse.map(typed).mkString("\n").ifEmpty("none")) == Correct())
        if (all.length > 1) assert(q.check(typed(all.head)).isInstanceOf[PartiallyCorrect])
        if (all.nonEmpty) assert(q.check("none").isInstanceOf[Incorrect])
      }
    }
  }

  test("assignments must mention exactly the formula's variables") {
    val pr = PropLogicProblem(Or(p, q)); pr.chooseSubproblems()
    val q1 = pr.GiveSatisfyingAssignments
    q1.ownFormula = And(p, q)
    assert(q1.check("p=true, q=true") == Correct())
    assert(q1.check("q = true, p = true") == Correct())
    assert(q1.check("p=true").isInstanceOf[MalformedInput])
    assert(!hint(q1.check("p=true, q=false")).contains("q=true")) // the hint does not give the answer away
  }

  test("validity and equivalence: yes or no") {
    for (_ <- 1 to runs) {
      val pr = fresh()
      val v = PropLogic.isValid(pr.CheckValidity.ownFormula)
      assert(pr.CheckValidity.check(if (v) "yes" else "no") == Correct())
      assert(pr.CheckValidity.check(if (v) "No" else "Yes").isInstanceOf[Incorrect])
      val e = PropLogic.isEquivalent(pr.formula, pr.CheckEquivalence.other)
      assert(pr.CheckEquivalence.check(if (e) "y" else "n") == Correct())
    }
    assert(fresh().CheckValidity.check("maybe").isInstanceOf[MalformedInput])
  }

  test("make it valid: every one-connective fix, however the brackets are written") {
    for (_ <- 1 to runs) {
      val pr = fresh()
      for (fix <- pr.MakeValid.fixes) assert(pr.MakeValid.check(typed(fix)) == Correct(), typed(fix))
    }
    // the page shows (p or not p) or q as p ∨ ¬p ∨ q; typed like that it must still count
    val pr = PropLogicProblem(Or(p, q)); pr.chooseSubproblems()
    val mv = pr.MakeValid
    mv.ownFormula = And(Or(p, Neg(p)), q)                       // not valid
    mv.fixes = PropLogic.oneConnectiveSwaps(mv.ownFormula).filter(PropLogic.isValid)
    assert(mv.fixes.contains(Or(Or(p, Neg(p)), q)))           // changing the top ∧ to ∨
    assert(mv.check("p or not p or q") == Correct())
    assert(hint(mv.check("p or q")).contains("not valid"))
    assert(hint(mv.check("p or not p")).contains("exactly one connective"))
  }

  private def hint(r: CheckResult): String = r match {
    case Incorrect(h) => h; case MalformedInput(h) => h; case PartiallyCorrect(_, h) => h; case o => o.toString
  }
  extension (s: String) private def ifEmpty(alt: String) = if (s.isEmpty) alt else s
}
