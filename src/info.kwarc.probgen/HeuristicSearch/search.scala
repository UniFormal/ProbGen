package info.kwarc.probgen

enum HeuristicSearchAlgorithm(val title: String, val points: Int) {
  case DepthFirst extends HeuristicSearchAlgorithm("Depth-first search", 1)
  case BreadthFirst extends HeuristicSearchAlgorithm("Breadth-first search", 1)
  case UniformCost extends HeuristicSearchAlgorithm("Uniform-cost search", 2)
  case Greedy extends HeuristicSearchAlgorithm("Greedy search", 2)
  case AStar extends HeuristicSearchAlgorithm("A*-search", 2)
}

object HeuristicSearch {
  /** Expand each node once. Continue after goal nodes to produce the requested trace. */
  def nextExpansions(
      graph: HeuristicSearchGraph,
      algorithm: HeuristicSearchAlgorithm,
      count: Int = 4
  ): List[String] = {
    case class Entry(name: String, depth: Int, cost: Int)
    val heuristics = graph.nodes.map(n => n.name -> n.heuristic).toMap
    val successors = graph.edges.groupBy(_.from)
    var frontier = List(Entry(graph.root, 0, 0))
    var expanded = Set.empty[String]
    val result = List.newBuilder[String]
    var found = 0

    def priority(entry: Entry): Int = algorithm match {
      case HeuristicSearchAlgorithm.BreadthFirst => entry.depth
      case HeuristicSearchAlgorithm.UniformCost => entry.cost
      case HeuristicSearchAlgorithm.Greedy => heuristics(entry.name).get
      case HeuristicSearchAlgorithm.AStar => entry.cost + heuristics(entry.name).get
      case HeuristicSearchAlgorithm.DepthFirst => 0
    }

    while (frontier.nonEmpty && found < count) {
      if (algorithm != HeuristicSearchAlgorithm.DepthFirst)
        frontier = frontier.sortBy(e => (priority(e), e.name))
      val current = frontier.head
      frontier = frontier.tail
      if (!expanded(current.name)) {
        expanded += current.name
        if (current.name != graph.root) {
          result += current.name
          found += 1
        }
        val children = successors.getOrElse(current.name, Nil).sortBy(_.to)
          .filterNot(e => expanded(e.to)).map { edge =>
            val cost = algorithm match {
              case HeuristicSearchAlgorithm.UniformCost | HeuristicSearchAlgorithm.AStar =>
                current.cost + edge.cost.get
              case _ => 0
            }
            Entry(edge.to, current.depth + 1, cost)
          }
        frontier = children ::: frontier
      }
    }
    result.result()
  }
}
