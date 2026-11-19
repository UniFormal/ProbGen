package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** Generated model answers, common input formats, and search regressions. */
class HeuristicSearchGradingTest extends AnyFunSuite {
  private val problems = List.fill(60)(HeuristicSearchProblemGenerator.make())

  test("generated graphs have three goal paths, positive costs, and admissible heuristics") {
    for (p <- problems) {
      val graph = p.graph
      val goal = graph.nodes.last.name
      def pathCosts(name: String): List[Int] =
        if (name == goal) List(0)
        else graph.edges.filter(_.from == name).flatMap(e => pathCosts(e.to).map(_ + e.cost.get))
      assert(pathCosts(graph.root).size >= 3)
      assert(graph.edges.forall(_.cost.exists((1 to 8).contains)))
      assert(graph.nodes.last.heuristic.contains(0))
      assert(graph.nodes.forall { n =>
        val exact = pathCosts(n.name).minOption
        graph.minimumCostsToGoal(n.name) == exact && exact.forall(n.heuristic.get <= _)
      })
    }
  }

  test("search answers accept compact letters, commas, lowercase, and whitespace") {
    for (p <- problems; q <- p.questions) {
      val expected = q.expected
      assert(expected.distinct == expected && !expected.contains("A"))
      assert(expected.size == 4 || expected.last == p.graph.nodes.last.name)
      assert(!expected.dropRight(1).contains(p.graph.nodes.last.name))
      for (answer <- List(expected.mkString, expected.mkString(", "),
        expected.mkString("[" , ",", "]"), expected.mkString(" \t").toLowerCase))
        assert(q.check(answer) == Correct(), answer)
      assert(q.check(expected.reverse.mkString).isInstanceOf[Incorrect])
      assert(q.check("AAAA").isInstanceOf[Incorrect])
      assert(q.check("Z").isInstanceOf[MalformedInput])
    }
  }

  test("h-star or admissibility comes first, consistency second, then five search questions") {
    for (p <- problems) {
      val subs = p.chooseSubproblems()
      assert(subs.map(_.pts) == List(2, 2, 1, 1, 2, 2, 2))
      assert(subs(1) == p.consistency && subs.drop(2) == p.questions)
      assert(Set("optimal-cost", "admissible-range").contains(subs.head.id))
      val q = subs.head.asInstanceOf[p.NodeQuestion]
      assert(q.node.nonEmpty && q.node != p.graph.nodes.last.name)
      assert(q.check(q.solution().toString) == Correct())
      val counterexamples = p.graph.inconsistentEdges.map(e => e.from + e.to)
      for (answer <- if (counterexamples.isEmpty) List("yes") else counterexamples)
        assert(p.consistency.check(answer) == Correct())
    }
  }

  test("search handles shared nodes, cheaper paths, alphabetical ties, and early goals") {
    import HeuristicSearchAlgorithm._
    val graph = HeuristicSearchGraph(
      List("A", "B", "C", "D", "E").map(n => HeuristicSearchNode(n, Some(0))),
      List(("A", "B", 5), ("A", "C", 1), ("C", "B", 1),
        ("B", "D", 1), ("C", "D", 2), ("D", "E", 1))
        .map { case (from, to, cost) => HeuristicSearchEdge(from, to, Some(cost)) }, "A")
    val answers = List(DepthFirst -> "BDE", BreadthFirst -> "BCDE",
      UniformCost -> "CBDE", Greedy -> "BCDE", AStar -> "CBDE")
    for ((algorithm, expected) <- answers) {
      assert(HeuristicSearch.nextExpansions(graph, algorithm).mkString == expected)
      assert(HeuristicSearch.nextExpansions(graph.copy(edges = graph.edges.reverse), algorithm).mkString == expected)
    }
  }

  test("heuristic grading handles finite ranges, unreachable goals, and consistency violations") {
    def problem(hB: Int) = HeuristicSearchProblem(
      HeuristicSearchGraph(
        List(("A", 1), ("B", hB), ("C", 1), ("D", 4), ("E", 0))
          .map { case (name, h) => HeuristicSearchNode(name, Some(h)) },
        List(("A", "B", 2), ("A", "D", 1), ("B", "C", 1), ("C", "E", 3))
          .map { case (from, to, cost) => HeuristicSearchEdge(from, to, Some(cost)) }, "A"),
      Map("A" -> (1.0, 0.0), "B" -> (0.0, 1.0), "C" -> (0.0, 2.0),
        "D" -> (1.0, 1.0), "E" -> (1.0, 3.0)))
    val p = problem(2)
    p.optimalCost.node = "B"
    p.admissibleRange.node = "B"
    assert(p.optimalCost.check("4") == Correct())
    assert(p.optimalCost.check("3").isInstanceOf[Incorrect])
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
    assert(p.consistency.check("YES") == Correct())
    assert(p.consistency.check("BC").isInstanceOf[Incorrect])
    val inconsistent = problem(4)
    assert(inconsistent.consistency.check("b c") == Correct())
    assert(inconsistent.consistency.check("yes").isInstanceOf[Incorrect])
    assert(inconsistent.consistency.check("CB").isInstanceOf[MalformedInput])
  }
}
