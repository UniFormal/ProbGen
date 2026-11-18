package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B6 (MDP): on random MDPs (grid and circular), model answers typed in are graded
  * correct, and typical wrong answers are not
  */
class MDPGradingTest extends AnyFunSuite {

  private val runs = 40
  private def problems = List.fill(runs)(MDPGenerator.make())
  private def show(x: Any) = ExprCompare.show(Expr.fromAny(x))

  test("states and actions: two sets, credit per set") {
    for (p <- problems) {
      val states  = p.mdp.states.map(show).mkString("{", ", ", "}")
      val actions = p.mdp.actions.map(a => show(p.mdp.actionName(a))).mkString("{", ", ", "}")
      val q = p.defineTuple
      assert(q.check(s"S = $states, A = $actions") == Correct(), s"S = $states, A = $actions")
      assert(q.check(s"A = $actions, S = $states") == Correct())
      val fewer = p.mdp.actions.tail.map(a => show(p.mdp.actionName(a))).mkString("{", ", ", "}")
      q.check(s"S = $states, A = $fewer") match {
        case PartiallyCorrect(f, _) => assert(f == 0.5)
        case other                  => fail(other.toString)
      }
    }
  }

  test("one step of value iteration / policy evaluation: the reward") {
    for (p <- problems) {
      def checkReward(q: p.Subproblem, r: Double) = {
        assert(q.check(Scoring.number(r)) == Correct())
        assert(q.check(s"U(s) = R(s) = ${Scoring.number(r)}") == Correct())
        assert(q.check(Scoring.number(r + 1)).isInstanceOf[Incorrect])
      }
      p.calcOneStepVI.init(); checkReward(p.calcOneStepVI, p.mdp.reward(p.calcOneStepVI.s))
      p.calcOneStepPI.init(); checkReward(p.calcOneStepPI, p.mdp.reward(p.calcOneStepPI.s))
    }
  }

  test("probability of a path: the product, written either way") {
    for (p <- problems) {
      val q = p.sequenceProbability
      q.init()
      val p1 = q.dist0.getOrElse(q.s1, 0.0)
      val p2 = q.dist1.getOrElse(q.s2, 0.0)
      assert(q.check(s"${Scoring.number(p1)} * ${Scoring.number(p2)}") == Correct())
      assert(q.check(Scoring.number(p1 * p2)) == Correct())
      if (p2 != 1.0) assert(q.check(Scoring.number(p1)).isInstanceOf[Incorrect])
    }
  }

  test("belief update: labelled values, states written in any form") {
    for (p <- problems) {
      val q = p.beliefUpdate
      q.init()
      val belief = p.newBelief(q.sA, q.sB, q.a)
      def answer(label: p.mdp.S => String) =
        belief.map { case (s, b) => s"b'(${label(s)}) = ${Scoring.number(b)}" }.mkString(", ")
      assert(q.check(answer(p.typedInside)) == Correct(), answer(p.typedInside))
      assert(q.check(answer(s => show(s))) == Correct())                    // b'((0, 1)) for a grid cell
      assert(q.check(answer(p.typedInside).replace("b'(", "b' (")) == Correct())
      if (belief.length > 1)
        assert(q.check(answer(p.typedInside).split(", b'").head).isInstanceOf[PartiallyCorrect])
      assert(belief.map(_._1).distinct.length == belief.length) // each state once
    }
  }
}
