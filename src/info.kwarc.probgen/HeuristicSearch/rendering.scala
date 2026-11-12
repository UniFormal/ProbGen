package info.kwarc.probgen

/** Coordinates are independent of graph data and need not lie in discrete layers. */
object HeuristicSearchLayout {
  val exam2024: Map[String, (Double, Double)] = Map(
    "A" -> (1.9, 0.0),
    "B" -> (0.9, 0.9), "C" -> (2.9, 0.9),
    "F" -> (1.9, 1.35),
    "D" -> (0.3, 1.8), "E" -> (1.3, 1.8), "G" -> (3.55, 1.8),
    "H" -> (0.0, 2.7), "I" -> (0.95, 2.7)
  )
}

case class SHeuristicSearchGraph(
    graph: HeuristicSearchGraph,
    positions: Map[String, (Double, Double)]
) extends SText {
  require(graph.nodes.map(_.name).distinct.size == graph.nodes.size, "Node names must be unique")
  require(positions.keySet == graph.nodes.map(_.name).toSet, "Every node needs a position")
  require(positions.values.forall { case (x, y) =>
    x.isFinite && y.isFinite && x >= 0 && y >= 0
  }, "Positions must be finite and nonnegative")
  require(graph.edges.forall(e => positions.contains(e.from) && positions.contains(e.to)),
    "Edges must refer to existing nodes")
  require(graph.edges.forall(e => positions(e.from) != positions(e.to)),
    "An edge needs distinct endpoint positions")

  private val diagram = SGraphDiagram[HeuristicSearchNode](
    graph.nodes.map { node =>
      val (x, y) = positions(node.name)
      (node, x, y)
    },
    graph.edges.map(e => (e.from, e.to)),
    _.name,
    _ => None,
    n => Some(n.heuristic),
    (from, to) => graph.edges.find(e => e.from == from && e.to == to).map(_.cost),
    directed = true
  )
  def toHTML = diagram.toHTML
  override def toString = diagram.toString
}
