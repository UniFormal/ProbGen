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
    assert(layout.graph.edges.forall(e => placed(e.from)._2 < placed(e.to)._2))
    val last = readingOrder.last
    assert(placed(last)._2 == placed.values.map(_._2).max)
    if (graph.nodes.size > 1) assert(placed(last)._1 == placed.values.map(_._1).max)
    val originalValues = graph.nodes.map(n => n.name -> n.heuristic).toMap
    val newValues = layout.graph.nodes.map(n => n.name -> n.heuristic).toMap
    assert(layout.graph.edges.map(e => (newValues(e.from), newValues(e.to), e.cost)).sorted ==
      graph.edges.map(e => (originalValues(e.from), originalValues(e.to), e.cost)).sorted)
    assert(layout == HeuristicSearchLayout(graph.copy(nodes = graph.nodes.reverse, edges = graph.edges.reverse))
      .copy(graph = layout.graph))
  }

  test("automatic layout supports shared successors and renames the exam graph") {
    verify(HeuristicSearchProblemGenerator.exam2024)
  }

  test("root isolation, ordering and downward edges hold for varied generated DAGs") {
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
