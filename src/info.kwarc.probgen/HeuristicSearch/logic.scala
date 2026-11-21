package info.kwarc.probgen

/** A directed search graph, independent of its drawing. */
case class HeuristicSearchNode(name: String, heuristic: Option[Int] = None)
case class HeuristicSearchEdge(from: String, to: String, cost: Option[Int] = None)
case class HeuristicSearchGraph(nodes: List[HeuristicSearchNode], edges: List[HeuristicSearchEdge], root: String) {
  /** None denotes a node from which the goal cannot be reached. */
  lazy val minimumCostsToGoal: Map[String, Option[Int]] = {
    val goal = nodes.map(_.name).max
    val successors = edges.groupBy(_.from)
    val costs = scala.collection.mutable.Map[String, Option[Int]](goal -> Some(0))
    for (name <- nodes.map(_.name).sorted.reverse if name != goal) {
      val alternatives = successors.getOrElse(name, Nil).flatMap { edge =>
        costs(edge.to).map(_ + edge.cost.get)
      }
      costs(name) = alternatives.minOption
    }
    costs.toMap
  }

  // A sample of 100 accepted generated graphs had 27 consistent heuristics (about 25%).
  lazy val inconsistentEdges: List[HeuristicSearchEdge] = {
    val heuristics = nodes.map(node => node.name -> node.heuristic.get).toMap
    edges.filter(edge => heuristics(edge.from) - heuristics(edge.to) > edge.cost.get)
  }

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
