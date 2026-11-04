package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B6 (Search): on random problems (with numbers and with letters as names), model
  * answers typed in are graded correct, and typical wrong answers are not
  */
class SearchGradingTest extends AnyFunSuite {

  private val runs = 60
  private def problems = List.fill(runs)(SearchProblemGenerator.make())
  private def typed(e: Expr) = ExprCompare.show(e)
  private def seq(p: ExpressionBasedDeterministicSearchProblem, as: List[Int]) =
    as.map(a => typed(p.actionName(a))).mkString(", ")

  test("give a solution: every solution is right, other sequences are not") {
    for (p <- problems if p.giveSolution.applicable()) {
      for (sol <- p.solutions) assert(p.giveSolution.check(seq(p, sol.actions)) == Correct(), seq(p, sol.actions))
      // brackets around the sequence are fine
      val first = p.solutions.head.actions
      assert(p.giveSolution.check("[" + seq(p, first) + "]") == Correct())
      // a sequence that can be carried out but ends elsewhere
      val wrongEnd = p.actions.flatMap(a => p.actions.map(b => List(a, b)))
        .find(as => p.apply(p.initial, as).nonEmpty && !p.check(as))
      wrongEnd.foreach(as => assert(p.giveSolution.check(seq(p, as)).isInstanceOf[Incorrect]))
      assert(p.giveSolution.check("nonsense").isInstanceOf[MalformedInput])
    }
  }

  test("give all solutions: any order, partial credit") {
    var tested = 0
    for (p <- problems if p.giveAllSolutions.applicable()) {
      tested += 1
      val all = p.solutions.map(s => seq(p, s.actions))
      assert(p.giveAllSolutions.check(all.mkString("; ")) == Correct())
      assert(p.giveAllSolutions.check(all.reverse.mkString("\n")) == Correct())
      assert(p.giveAllSolutions.check(all.head).isInstanceOf[PartiallyCorrect])
    }
    info(s"$tested problems had 2-3 solutions")
  }

  test("states reached by an action sequence") {
    for (p <- problems) {
      val q = p.applyAction
      q.init()
      val states = q.result.map(s => typed(p.stateName(s)))
      assert(q.check(states.mkString("{", ", ", "}")) == Correct(), states)
      assert(q.check(states.mkString(", ")) == Correct())
      val other = p.states.find(s => !q.result.contains(s)).get
      assert(q.check((states :+ typed(p.stateName(other))).mkString(", ")).isInstanceOf[PartiallyCorrect])
    }
  }

  test("an inapplicable (state, action) pair: checked with the transition model") {
    // The generator never produces this question: it shows a problem arithmetically only
    // if the transition is taken modulo the number of states, and then every action is
    // applicable. So the grader is tested on a hand-made problem: s + a without modulo.
    val handMade = ExpressionBasedDeterministicSearchProblem(
      5, List(1, 2), Plus(Var("s"), Var("a")), 0, Equals(Var("s"), DInt(4)), 6, presentArithmetically = true)
    assert(handMade.actionNotApplicable.applicable())
    var tested = 0
    for (p <- handMade :: problems if p.actionNotApplicable.applicable()) {
      tested += 1
      val q = p.actionNotApplicable
      for ((s, a) <- p.inapplicableActions) assert(q.check(s"($s, $a)") == Correct())
      val (s, a) = p.inapplicableActions.head
      assert(q.check(s"$s, $a") == Correct())                         // brackets optional
      val applicable = p.states.flatMap(s => p.actions.map(a => (s, a))).find { case (s, a) => p.trans(s, a).nonEmpty }
      applicable.foreach { case (s, a) => assert(q.check(s"($s, $a)").isInstanceOf[Incorrect]) }
      assert(q.check("(999, 1)").isInstanceOf[MalformedInput])         // not a state
    }
    info(s"$tested problems had an inapplicable action")
  }
}
