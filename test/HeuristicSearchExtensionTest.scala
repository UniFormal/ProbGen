package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

class HeuristicSearchExtensionTest extends AnyFunSuite {
  private def problem(hB: Int): HeuristicSearchProblem = {
    val graph = HeuristicSearchGraph(
      List(("A", 1), ("B", hB), ("C", 1), ("D", 4), ("E", 0))
        .map { case (name, h) => HeuristicSearchNode(name, Some(h)) },
      List(("A", "B", 2), ("A", "D", 1), ("B", "C", 1), ("C", "E", 3))
        .map { case (from, to, c) => HeuristicSearchEdge(from, to, Some(c)) }, "A")
    HeuristicSearchProblem(graph, Map("A" -> (1.0, 0.0), "B" -> (0.0, 1.0),
      "C" -> (0.0, 2.0), "D" -> (1.0, 1.0), "E" -> (1.0, 3.0)))
  }

  test("h-star and admissible ranges handle finite costs and dead ends") {
    val p = problem(2)
    p.optimalCost.node = "B"
    assert(p.optimalCost.check("4") == Correct())
    assert(p.optimalCost.check("3").isInstanceOf[Incorrect])
    p.admissibleRange.node = "B"
    assert(p.admissibleRange.check(" 0 - 4 ") == Correct())
    assert(p.admissibleRange.check("1-4").isInstanceOf[Incorrect])
    assert(p.admissibleRange.check("0-5").isInstanceOf[Incorrect])
    assert(p.admissibleRange.check("4").isInstanceOf[MalformedInput])
    p.optimalCost.node = "D"
    p.admissibleRange.node = "D"
    for (infinity <- List("infinity", "inf", "∞")) {
      assert(p.optimalCost.check(infinity) == Correct())
      assert(p.admissibleRange.check("0-" + infinity) == Correct())
    }
    assert(p.optimalCost.check("0").isInstanceOf[Incorrect])
    assert(p.admissibleRange.check("0-8").isInstanceOf[Incorrect])
  }

  test("consistency accepts any actual counterexample and rejects reversed edges") {
    val consistent = problem(2)
    assert(consistent.graph.inconsistentEdges.isEmpty)
    assert(consistent.consistency.check("YES") == Correct())
    assert(consistent.consistency.check("BC").isInstanceOf[Incorrect])
    val inconsistent = problem(4)
    assert(inconsistent.graph.inconsistentEdges.map(e => e.from + e.to) == List("BC"))
    assert(inconsistent.consistency.check("b c") == Correct())
    assert(inconsistent.consistency.check("yes").isInstanceOf[Incorrect])
    assert(inconsistent.consistency.check("CB").isInstanceOf[MalformedInput])
    assert(inconsistent.consistency.check("no").isInstanceOf[MalformedInput])
  }

  test("each exercise selects exactly one extension question and excludes the goal") {
    val selectedIds = scala.collection.mutable.Set.empty[String]
    for (_ <- 0 until 60) {
      val p = HeuristicSearchProblemGenerator.make()
      val subs = p.chooseSubproblems()
      val extra = subs.filterNot(p.questions.contains)
      assert(subs.size == 6 && extra.size == 1)
      selectedIds += extra.head.id
      extra.head match {
        case q: p.NodeQuestion =>
          assert(q.node.nonEmpty && q.node != p.graph.nodes.last.name)
          assert(q.check(q.solution().toString) == Correct())
        case _ =>
          val answer = p.graph.inconsistentEdges.headOption
            .map(e => e.from + e.to).getOrElse("yes")
          assert(p.consistency.check(answer) == Correct())
          for (edge <- p.graph.inconsistentEdges)
            assert(p.consistency.check(edge.from + edge.to) == Correct())
      }
    }
    assert(selectedIds.toSet == Set("optimal-cost", "admissible-range", "consistency"))
  }
}
