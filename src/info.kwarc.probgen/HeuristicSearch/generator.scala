package info.kwarc.probgen

object HeuristicSearchProblemGenerator extends ProblemGenerator[HeuristicSearchProblem] {
  /** The 2024/25 exam graph, including its shared successors F and I. */
  val exam2425 = HeuristicSearchGraph(
    List(("A", 8), ("B", 6), ("C", 8), ("D", 5), ("E", 4),
      ("F", 5), ("G", 2), ("H", 0), ("I", 1))
      .map { case (name, h) => HeuristicSearchNode(name, h) },
    List(("A", "B", 1), ("A", "C", 2), ("A", "F", 4),
      ("B", "D", 5), ("B", "E", 3), ("C", "F", 1), ("C", "G", 5),
      ("D", "H", 1), ("D", "I", 1), ("E", "I", 2))
      .map { case (from, to, cost) => HeuristicSearchEdge(from, to, cost) },
    "A"
  )

  /** The 2023/24 exam graph, using the node names from the original figure. */
  val exam2324 = HeuristicSearchGraph(
    List(("A", 10), ("B", 4), ("C", 6), ("D", 7), ("E", 7),
      ("F", 6), ("G", 4), ("H", 3), ("I", 0))
      .map { case (name, h) => HeuristicSearchNode(name, h) },
    List(("A", "B", 6), ("A", "C", 1), ("B", "C", 3), ("B", "E", 1),
      ("C", "F", 2), ("C", "G", 6), ("D", "H", 1), ("E", "D", 1),
      ("E", "F", 2), ("F", "G", 1), ("F", "H", 1), ("F", "I", 8),
      ("G", "I", 4), ("H", "I", 5))
      .map { case (from, to, cost) => HeuristicSearchEdge(from, to, cost) },
    "A"
  )

  def make(): HeuristicSearchProblem = {
    val layout = HeuristicSearchLayout(exam2324)
    HeuristicSearchProblem(layout.graph, layout.positions)
  }
}
