package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** the propositional-logic solver that the Logic problem's answers and grading rely on */
class LogicSolverTest extends AnyFunSuite {
  private val p = Var("p")
  private val q = Var("q")

  test("assignments enumerates all 2^n truth assignments") {
    assert(PropLogic.assignments(List("p", "q")).length == 4)
    assert(PropLogic.assignments(Nil) == List(Context()))
  }

  test("a Context can bind a variable to a Boolean") {
    assert(Context()("p" -> true)("p") == DBool(true))
  }

  test("validity") {
    assert(PropLogic.isValid(Or(p, Neg(p))))
    assert(!PropLogic.isValid(p))
    assert(!PropLogic.isValid(And(p, q)))
  }

  test("equivalence") {
    assert(PropLogic.isEquivalent(Implies(p, q), Or(Neg(p), q)))
    assert(!PropLogic.isEquivalent(p, q))
    assert(!PropLogic.isEquivalent(Implies(p, q), And(p, q)))
  }

  test("satisfying and falsifying assignments") {
    assert(PropLogic.findAllAssignments(And(p, q), true).length == 1)
    assert(PropLogic.findAllAssignments(And(p, q), false).length == 3)
  }

  test("negated variables count as literals in CNF and DNF") {
    assert(PropLogic.isCNF(And(Or(Neg(p), q), p)))
    assert(PropLogic.isDNF(Or(And(Neg(p), q), Neg(q))))
    assert(!PropLogic.isCNF(Neg(And(p, q))))
  }

  test("toCNF and toDNF produce equivalent formulas of the right shape") {
    val f = Implies(Or(p, q), And(p, Neg(q)))
    val cnf = PropLogic.toCNF(f)
    val dnf = PropLogic.toDNF(f)
    assert(PropLogic.isCNF(cnf) && PropLogic.isEquivalent(f, cnf))
    assert(PropLogic.isDNF(dnf) && PropLogic.isEquivalent(f, dnf))
  }
}
