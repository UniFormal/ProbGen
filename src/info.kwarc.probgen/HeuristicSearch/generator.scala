package info.kwarc.probgen

object HeuristicSearchProblemGenerator extends ProblemGenerator[HeuristicSearchProblem] {
  /** A fixed tree for the first rendering example. */
  def make(): HeuristicSearchProblem = HeuristicSearchProblem(
    HeuristicSearchGraph(
      List(("A", 8), ("B", 6), ("C", 4), ("D", 3), ("E", 0), ("F", 0))
        .map { case (name, h) => HeuristicSearchNode(name, h) },
      List(("A", "B", 2), ("A", "C", 4), ("B", "D", 3), ("B", "E", 6), ("C", "F", 4))
        .map { case (from, to, cost) => HeuristicSearchEdge(from, to, cost) },
      "A"
    )
  )
}
