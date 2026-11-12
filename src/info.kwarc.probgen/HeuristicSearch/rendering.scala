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

  // Long edges travel through separate lanes beyond the rightmost node.
  private val longEdges = graph.edges.filter(e => positions(e.to)._2 - positions(e.from)._2 > 1.31)
  private val bends = longEdges.zipWithIndex.map { case (edge, index) =>
    val (fromX, fromY) = positions(edge.from)
    val (toX, toY) = positions(edge.to)
    val lane = positions.values.map(_._1).max + 0.7 + index * 0.45
    (edge.from, edge.to) -> List(
      (fromX, fromY + 0.45), (lane, fromY + 0.45),
      (lane, toY - 0.45), (toX, toY - 0.45)
    )
  }.toMap

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
    directed = true,
    bends = bends
  )
  def toHTML = diagram.toHTML
  override def toString = diagram.toString
}
