package info.kwarc.probgen

object HeuristicSearchProblemGenerator extends ProblemGenerator[HeuristicSearchProblem] {
  /** The 2024/25 exam graph, including its shared successors F and I. */
  val exam2024 = HeuristicSearchGraph(
    List(("A", 8), ("B", 6), ("C", 8), ("D", 5), ("E", 4),
      ("F", 5), ("G", 2), ("H", 0), ("I", 1))
      .map { case (name, h) => HeuristicSearchNode(name, h) },
    List(("A", "B", 1), ("A", "C", 2), ("A", "F", 4),
      ("B", "D", 5), ("B", "E", 3), ("C", "F", 1), ("C", "G", 5),
      ("D", "H", 1), ("D", "I", 1), ("E", "I", 2))
      .map { case (from, to, cost) => HeuristicSearchEdge(from, to, cost) },
    "A"
  )

  def make(): HeuristicSearchProblem = {
    val layout = HeuristicSearchLayout(exam2024)
    HeuristicSearchProblem(layout.graph, layout.positions)
  }
}
