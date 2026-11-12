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
    // At least four candidates remain across the two non-root pairs of rows.
    val extra = Generator.chooseSome(unused, atLeast = 2, atMost = 3, allowRep = false)
    HeuristicSearchGraph(names.map(name => HeuristicSearchNode(name)).toList,
      (backbone ++ extra).sortBy(e => (e.from, e.to)), names.head)
  }

  def fromGraph(graph: HeuristicSearchGraph): HeuristicSearchProblem = {
    val layout = HeuristicSearchLayout(graph)
    HeuristicSearchProblem(layout.graph, layout.positions)
  }

  def make(): HeuristicSearchProblem = fromGraph(randomGraph())
}
