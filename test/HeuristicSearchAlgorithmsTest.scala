package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

class HeuristicSearchAlgorithmsTest extends AnyFunSuite {
  test("all five algorithms reproduce the 2024/25 exam answers") {
    val problem = HeuristicSearchExamples.exam2425
    val answers = List("BDHI", "BCFD", "BCFE", "FBEI", "BEIF")
    assert(problem.chooseSubproblems().map(_.pts) == List(1, 1, 2, 2, 2))
    for ((question, answer) <- problem.questions.zip(answers)) {
      assert(question.expected.mkString == answer)
      assert(question.check(answer.toList.mkString(", ")) == Correct())
      assert(question.check(answer.toLowerCase.toList.mkString("[", ", ", "]")) == Correct())
      assert(question.check("A, A, A, A").isInstanceOf[Incorrect])
      assert(question.check("Z, B, C, D").isInstanceOf[MalformedInput])
    }
  }

  test("shared nodes expand once, cheapest frontier paths win, and ties are alphabetical") {
    import HeuristicSearchAlgorithm._
    val graph = HeuristicSearchGraph(
      List("A", "B", "C", "D", "E").map(n => HeuristicSearchNode(n, Some(0))),
      List(("A", "B", 5), ("A", "C", 1), ("C", "B", 1),
        ("B", "D", 1), ("C", "D", 2), ("D", "E", 1))
        .map { case (from, to, cost) => HeuristicSearchEdge(from, to, Some(cost)) }, "A")
    assert(HeuristicSearch.nextExpansions(graph, UniformCost) == List("C", "B", "D", "E"))
    assert(HeuristicSearch.nextExpansions(graph, AStar) == List("C", "B", "D", "E"))
    assert(HeuristicSearch.nextExpansions(graph, BreadthFirst) == List("B", "C", "D", "E"))
    assert(HeuristicSearch.nextExpansions(graph, DepthFirst) == List("B", "D", "E", "C"))
    assert(HeuristicSearch.nextExpansions(graph.copy(edges = graph.edges.reverse), UniformCost) ==
      HeuristicSearch.nextExpansions(graph, UniformCost))
    assert(HeuristicSearch.nextExpansions(graph, Greedy, 20) == List("B", "C", "D", "E"))
  }

  test("generated questions have four unique expansions and accept their computed solutions") {
    for (_ <- 0 until 30) {
      val problem = HeuristicSearchProblemGenerator.make()
      assert(problem.chooseSubproblems().size == 5)
      for (question <- problem.questions) {
        assert(question.expected.size == 4)
        assert(question.expected.distinct == question.expected)
        assert(!question.expected.contains("A"))
        assert(question.check(question.expected.mkString(", ")) == Correct())
        assert(question.check(question.expected.reverse.mkString(", ")).isInstanceOf[Incorrect])
      }
    }
  }
}
