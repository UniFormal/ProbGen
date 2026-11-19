package info.kwarc.probgen

object HeuristicSearchExamples {
  /** Original exam names and weights, positioned directly for comparison. */
  def exam2425: HeuristicSearchProblem = {
    val nodes = List(("A", 8), ("B", 6), ("C", 8), ("D", 5), ("E", 4),
      ("F", 5), ("G", 2), ("H", 0), ("I", 1))
      .map { case (name, h) => HeuristicSearchNode(name, Some(h)) }
    val edges = List(("A", "B", 1), ("A", "C", 2), ("A", "F", 4),
      ("B", "D", 5), ("B", "E", 3), ("C", "F", 1), ("C", "G", 5),
      ("D", "H", 1), ("D", "I", 1), ("E", "I", 2))
      .map { case (from, to, cost) => HeuristicSearchEdge(from, to, Some(cost)) }
    val positions = Map(
      "A" -> (1.9, 0.0), "B" -> (0.9, 0.9), "C" -> (2.9, 0.9),
      "F" -> (1.9, 1.35), "D" -> (0.3, 1.8), "E" -> (1.3, 1.8),
      "G" -> (3.55, 1.8), "H" -> (0.0, 2.7), "I" -> (0.95, 2.7)
    )
    HeuristicSearchProblem(HeuristicSearchGraph(nodes, edges, "A"), positions)
  }
}
