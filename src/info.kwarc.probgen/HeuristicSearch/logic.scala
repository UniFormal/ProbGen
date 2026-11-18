package info.kwarc.probgen

/** A directed search graph, independent of its drawing. */
case class HeuristicSearchNode(name: String, heuristic: Option[Int] = None)
case class HeuristicSearchEdge(from: String, to: String, cost: Option[Int] = None)
case class HeuristicSearchGraph(nodes: List[HeuristicSearchNode], edges: List[HeuristicSearchEdge], root: String) {
  /** Generated graphs are in alphabetical topological order. Counts saturate at three. */
  def hasThreePathsToGoal: Boolean = {
    val goal = nodes.map(_.name).max
    val successors = edges.groupMap(_.from)(_.to)
    val counts = scala.collection.mutable.Map(goal -> 1)
    for (name <- nodes.map(_.name).sorted.reverse if name != goal) {
      counts(name) = math.min(3, successors.getOrElse(name, Nil).map(counts(_)).sum)
    }
    counts(root) >= 3
  }
}
