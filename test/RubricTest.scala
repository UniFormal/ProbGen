package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B4: the free-text questions are self-graded with a rubric */
class RubricTest extends AnyFunSuite {

  /** the six free-text questions, from a fresh Search and a fresh MDP problem */
  private def freeTextQuestions: List[(String, Problem[?]#Subproblem)] = {
    val s = SearchProblemGenerator.make()
    val m = MDPGenerator.make()
    List(
      "Search: fully observable"       -> s.whyPO,
      "Search: deterministic"          -> s.whyDet,
      "Search: every action applicable" -> s.allActionsApplicable,
      "MDP: Bellman optimality"        -> m.bellmanEqVI,
      "MDP: Bellman expectation"       -> m.bellmanEqPI,
      "MDP: deriving the policy"       -> m.policyImprovement)
  }

  test("every free-text question has a rubric whose points add up to the question's") {
    for ((name, q) <- freeTextQuestions) {
      assert(q.rubric.isDefined, name)
      assert(q.rubric.get.totalPoints == q.pts, s"$name: rubric ${q.rubric.get.totalPoints} vs ${q.pts} pts")
      assert(q.grader.isEmpty, name) // graded by the rubric, not automatically
    }
  }

  test("an answer leads to self-grading: model answer and one checkbox per criterion") {
    for ((name, q) <- freeTextQuestions) {
      q.check("my explanation") match {
        case r @ NeedsHumanGrading(_, rubric, uid) =>
          assert(uid == q.uid, name)
          val html = r.messageHtml(q.pts)
          assert(html.contains("Model answer"), name)
          assert("type='checkbox'".r.findAllIn(html).length == rubric.classes.length, name)
          assert(html.contains(s"selfGrade('${q.uid}', 0, this.checked)"), name)
          assert(html.contains(s"id='sg-${q.uid}'"), name)
        case other => fail(s"$name: $other")
      }
      assert(q.check("  ").isInstanceOf[MalformedInput], name) // an empty answer is still not an attempt
    }
  }

  test("self-graded points: share of the question's points, and how they are shown") {
    assert(SelfGraded(2, 2) == SelfGraded(2, 2) && SelfGraded(2, 2).fraction == 1.0)
    assert(SelfGraded(2, 2).cssClass == "correct")
    assert(SelfGraded(1, 3).cssClass == "partial" && Math.abs(SelfGraded(1, 3).fraction - 1.0 / 3) < 1e-9)
    assert(SelfGraded(0, 2).cssClass == "wrong" && SelfGraded(0, 2).fraction == 0.0)
    assert(SelfGraded(1, 3).messageHtml(3).contains("1 of 3 points"))
  }

  test("the policy model answer takes the argmax over actions, with the sum over s' inside") {
    val m = MDPGenerator.make()
    val tex = m.policyImprovement.solution().toString
    assert(tex.contains("\\argmax_{\\inset{a}{A}}"), tex)
    assert(tex.contains("\\sum_{\\inset{s'}{S}}"), tex)
  }
}
