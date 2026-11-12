package info.kwarc.probgen

object HeuristicSearchProblemGenerator extends ProblemGenerator[HeuristicSearchProblem] {
  /** The 2024/25 exam graph, including its shared successors F and I. */
  val exam2425 = HeuristicSearchGraph(
    List(("A", 8), ("B", 6), ("C", 8), ("D", 5), ("E", 4),
      ("F", 5), ("G", 2), ("H", 0), ("I", 1))
      .map { case (name, h) => HeuristicSearchNode(name, Some(h)) },
    List(("A", "B", 1), ("A", "C", 2), ("A", "F", 4),
      ("B", "D", 5), ("B", "E", 3), ("C", "F", 1), ("C", "G", 5),
      ("D", "H", 1), ("D", "I", 1), ("E", "I", 2))
      .map { case (from, to, cost) => HeuristicSearchEdge(from, to, Some(cost)) },
    "A"
  )

  /** The 2023/24 exam graph, using the node names from the original figure. */
  val exam2324 = HeuristicSearchGraph(
    List(("A", 10), ("B", 4), ("C", 6), ("D", 7), ("E", 7),
      ("F", 6), ("G", 4), ("H", 3), ("I", 0))
      .map { case (name, h) => HeuristicSearchNode(name, Some(h)) },
    List(("A", "B", 6), ("A", "C", 1), ("B", "C", 3), ("B", "E", 1),
      ("C", "F", 2), ("C", "G", 6), ("D", "H", 1), ("E", "D", 1),
      ("E", "F", 2), ("F", "G", 1), ("F", "H", 1), ("F", "I", 8),
      ("G", "I", 4), ("H", "I", 5))
      .map { case (from, to, cost) => HeuristicSearchEdge(from, to, Some(cost)) },
    "A"
  )

  /** The 2022/23 exam graph with F → E and I → G omitted to make it acyclic. */
  val exam2223 = HeuristicSearchGraph(
    List(("A", 10), ("B", 8), ("C", 9), ("D", 7), ("E", 5),
      ("F", 4), ("G", 3), ("H", 4), ("I", 0))
      .map { case (name, h) => HeuristicSearchNode(name, Some(h)) },
    List(("A", "B", 6), ("A", "C", 3), ("B", "D", 3), ("B", "E", 1),
      ("C", "F", 2), ("C", "G", 5), ("D", "H", 1), ("E", "C", 1),
      ("F", "H", 5), ("F", "I", 8), ("G", "F", 1), ("H", "I", 5))
      .map { case (from, to, cost) => HeuristicSearchEdge(from, to, Some(cost)) },
    "A"
  )

  def randomGraph(): HeuristicSearchGraph = {
    val widths = 1 :: List.fill(3)(Generator.chooseInt(2, 3))
    val boundaries = widths.scanLeft(0)(_ + _)
    val names = (0 until boundaries.last).map(i => ('A' + i).toChar.toString)
    val rows = boundaries.zip(boundaries.tail).map { case (start, end) => names.slice(start, end) }
    val consecutiveRows = rows.zip(rows.tail)
    val backbone = consecutiveRows.flatMap { case (parents, children) =>
      children.map(child => HeuristicSearchEdge(Generator.choose(parents), child))
    }
    val unused = consecutiveRows.flatMap { case (parents, children) =>
      for (parent <- parents; child <- children) yield HeuristicSearchEdge(parent, child)
    }.filterNot(backbone.contains)
    val sideways = rows.slice(1, 3).map { row =>
      val from = Generator.chooseInt(0, row.size - 2)
      HeuristicSearchEdge(row(from), row(from + 1))
    }
    val extra = if (Generator.chooseBoolean(0.5)) List(Generator.choose(unused)) else Nil
    HeuristicSearchGraph(names.map(name => HeuristicSearchNode(name)).toList,
      (backbone ++ sideways ++ extra).sortBy(e => (e.from, e.to)), names.head)
  }

  def fromGraph(graph: HeuristicSearchGraph): HeuristicSearchProblem = {
    val layout = HeuristicSearchLayout(graph)
    HeuristicSearchProblem(layout.graph, layout.positions)
  }

  def make(): HeuristicSearchProblem = {
    // Judge the final layout, since a connection within a generation row can
    // end up diagonal after the layout algorithm rearranges the nodes.
    def quality(problem: HeuristicSearchProblem): (Int, Int) = {
      val rows = problem.positions.values.map(_._2).toSet.toList.sorted.zipWithIndex.toMap
      val spans = problem.graph.edges.map { edge =>
        rows(problem.positions(edge.to)._2) - rows(problem.positions(edge.from)._2)
      }
      (math.min(2, spans.count(_ == 0)), -spans.count(_ > 1))
    }

    var best = fromGraph(randomGraph())
    var bestQuality = quality(best)
    val alternatives = Iterator.fill(3)(fromGraph(randomGraph()))
    while (bestQuality != (2, 0) && alternatives.hasNext) {
      val candidate = alternatives.next()
      val candidateQuality = quality(candidate)
      if (Ordering[(Int, Int)].gt(candidateQuality, bestQuality)) {
        best = candidate
        bestQuality = candidateQuality
      }
    }
    best
  }
}
