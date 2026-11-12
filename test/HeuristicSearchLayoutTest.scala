package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

class HeuristicSearchLayoutTest extends AnyFunSuite {
  private def verify(graph: HeuristicSearchGraph): Unit = {
    val layout = HeuristicSearchLayout(graph)
    val placed = layout.positions
    val readingOrder = placed.toList.sortBy { case (_, (x, y)) => (y, x) }.map(_._1)
    assert(readingOrder == graph.nodes.indices.map(i => ('A' + i).toChar.toString).toList)
    assert(placed.filter(_._2._2 == 0).keySet == Set("A"))
    assert(layout.graph.root == "A")
    assert(layout.graph.edges.forall { e =>
      val (fromX, fromY) = placed(e.from)
      val (toX, toY) = placed(e.to)
      fromY < toY || (fromY == toY && toX > fromX &&
        !placed.values.exists { case (x, y) => y == fromY && fromX < x && x < toX })
    })
    val last = readingOrder.last
    val bottom = placed.values.map(_._2).max
    val bottomRow = placed.values.filter(_._2 == bottom)
    assert(placed(last) == (bottomRow.map(_._1).max, bottom))
    val originalValues = graph.nodes.map(n => n.name -> n.heuristic).toMap
    val newValues = layout.graph.nodes.map(n => n.name -> n.heuristic).toMap
    assert(layout.graph.edges.map(e => (newValues(e.from), newValues(e.to), e.cost)).sorted ==
      graph.edges.map(e => (originalValues(e.from), originalValues(e.to), e.cost)).sorted)
    assert(layout == HeuristicSearchLayout(graph.copy(nodes = graph.nodes.reverse, edges = graph.edges.reverse))
      .copy(graph = layout.graph))
  }

  test("the 2024/25 layout keeps the root's shared successor in the second row") {
    val graph = HeuristicSearchProblemGenerator.exam2425
    verify(graph)
    val layout = HeuristicSearchLayout(graph)
    val secondRow = layout.positions.values.map(_._2).filter(_ > 0).min
    assert(layout.graph.edges.filter(_.from == "A").forall(e => layout.positions(e.to)._2 == secondRow))
  }

  test("the 2023/24 exam graph renders all weighted edges") {
    val graph = HeuristicSearchProblemGenerator.exam2324
    verify(graph)
    val problem = HeuristicSearchLayout(graph)
    assert(problem.graph.edges.size == 14)
    assert(problem.positions.values.map(_._2).toSet.size <= 4)
    assert(problem.graph.edges.exists(e => problem.positions(e.from)._2 == problem.positions(e.to)._2))
    val diagram = SHeuristicSearchGraph(problem.graph, problem.positions)
    assert("<polygon ".r.findAllIn(diagram.toHTML).size == 14)
    assert(diagram.toString.linesIterator.count(_.startsWith("\\draw[->]")) == 14)
  }

  test("the 2022/23 exam variant is acyclic and keeps I as a sink") {
    val graph = HeuristicSearchProblemGenerator.exam2223
    assert(!graph.edges.exists(e => e.from == "F" && e.to == "E"))
    assert(!graph.edges.exists(_.from == "I"))
    verify(graph)
  }

  test("a narrow bottom row is centered with the terminal at its right end") {
    val nodes = ('A' to 'F').map(n => HeuristicSearchNode(n.toString, 0)).toList
    val edges = List(("A", "B"), ("A", "C"), ("A", "D"), ("B", "E"), ("C", "F"))
      .map { case (from, to) => HeuristicSearchEdge(from, to, 1) }
    val layout = HeuristicSearchLayout(HeuristicSearchGraph(nodes, edges, "A"))
    val (goalX, bottom) = layout.positions("F")
    val bottomXs = layout.positions.values.collect { case (x, y) if y == bottom => x }.toList
    val allXs = layout.positions.values.map(_._1)
    assert(bottomXs.size == 2)
    assert(goalX == bottomXs.max)
    assert(goalX < allXs.max)
    assert(math.abs(bottomXs.min + bottomXs.max - allXs.min - allXs.max) < 1e-9)
  }

  test("root isolation, ordering and edge direction hold for varied generated DAGs") {
    val random = new scala.util.Random(42)
    for (size <- 1 to 26; _ <- 0 until 10) {
      val nodes = (0 until size).map(i => HeuristicSearchNode(s"node$i", i)).toList
      val tree = (1 until size).map(i => (random.nextInt(i), i)).toSet
      val extra = (for (a <- 0 until size; b <- a + 1 until size if random.nextDouble() < 0.08)
        yield (a, b)).toSet
      val edges = (tree ++ extra).toList.sorted.map { case (a, b) =>
        HeuristicSearchEdge(s"node$a", s"node$b", 1 + random.nextInt(9))
      }
      verify(HeuristicSearchGraph(nodes, edges, "node0"))
    }
  }

  test("cycles and unreachable nodes fail clearly") {
    val nodes = List(HeuristicSearchNode("A", 1), HeuristicSearchNode("B", 0))
    intercept[IllegalArgumentException] {
      HeuristicSearchLayout(HeuristicSearchGraph(nodes, Nil, "A"))
    }
    intercept[IllegalArgumentException] {
      HeuristicSearchLayout(HeuristicSearchGraph(nodes,
        List(HeuristicSearchEdge("A", "B", 1), HeuristicSearchEdge("B", "B", 1)), "A"))
    }
  }
}
