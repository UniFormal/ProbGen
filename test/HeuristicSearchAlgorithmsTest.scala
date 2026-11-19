package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

class HeuristicSearchAlgorithmsTest extends AnyFunSuite {
  test("the 2024/25 example uses computed answers truncated at goal I") {
    val problem = HeuristicSearchExamples.exam2425
    val answers = List("BDHI", "BCFD", "BCFE", "FBEI", "BEI")
    assert(problem.chooseSubproblems().map(_.pts) == List(1, 1, 2, 2, 2, 2))
    for ((question, answer) <- problem.questions.zip(answers)) {
      assert(question.expected.mkString == answer)
      assert(question.check(answer.toList.mkString(", ")) == Correct())
      assert(question.check(answer) == Correct())
      assert(question.check(answer.toLowerCase.toList.mkString(" \t", " \n ", " ")) == Correct())
      assert(question.check(answer.toList.mkString(" ", " ,\t ", " ")) == Correct())
      assert(question.check(answer.toList.mkString("\u00a0")) == Correct())
      assert(question.check(answer.reverse).isInstanceOf[Incorrect])
      assert(question.check(answer.toLowerCase.toList.mkString("[", ", ", "]")) == Correct())
      assert(question.check("A, A, A, A").isInstanceOf[Incorrect])
      assert(question.check("Z, B, C, D").isInstanceOf[MalformedInput])
    }
  }

  test("every algorithm stops when it selects the goal, including before four expansions") {
    val graph = HeuristicSearchGraph(
      List("A", "B", "C", "D", "E").map(n => HeuristicSearchNode(n, Some(0))),
      List(("A", "B", 1), ("A", "C", 5), ("B", "E", 1), ("C", "D", 1))
        .map { case (from, to, cost) => HeuristicSearchEdge(from, to, Some(cost)) }, "A")
    for (algorithm <- HeuristicSearchAlgorithm.values) {
      val trace = HeuristicSearch.nextExpansions(graph, algorithm, 20)
      assert(trace.last == "E")
      assert(trace.size < 5)
    }
    assert(HeuristicSearch.nextExpansions(graph, HeuristicSearchAlgorithm.UniformCost) == List("B", "E"))
    assert(HeuristicSearch.nextExpansions(graph, HeuristicSearchAlgorithm.BreadthFirst) ==
      List("B", "C", "D", "E"))
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
    assert(HeuristicSearch.nextExpansions(graph, DepthFirst) == List("B", "D", "E"))
    assert(HeuristicSearch.nextExpansions(graph.copy(edges = graph.edges.reverse), UniformCost) ==
      HeuristicSearch.nextExpansions(graph, UniformCost))
    assert(HeuristicSearch.nextExpansions(graph, Greedy, 20) == List("B", "C", "D", "E"))
  }

  test("generated questions stop at the goal or four expansions and accept their solutions") {
    for (_ <- 0 until 30) {
      val problem = HeuristicSearchProblemGenerator.make()
      assert(problem.chooseSubproblems().size == 6)
      for (question <- problem.questions) {
        assert((1 to 4).contains(question.expected.size))
        assert(question.expected.size == 4 || question.expected.last == problem.graph.nodes.last.name)
        assert(!question.expected.dropRight(1).contains(problem.graph.nodes.last.name))
        assert(question.expected.distinct == question.expected)
        assert(!question.expected.contains("A"))
        assert(question.check(question.expected.mkString(", ")) == Correct())
        assert(question.check(question.expected.reverse.mkString(", ")).isInstanceOf[Incorrect])
      }
    }
  }
}
