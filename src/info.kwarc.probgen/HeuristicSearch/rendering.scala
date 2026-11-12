package info.kwarc.probgen

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
    _.heuristic,
    (from, to) => graph.edges.find(e => e.from == from && e.to == to).flatMap(_.cost),
    directed = true
  )
  def toHTML = diagram.toHTML
  override def toString = diagram.toString
}
