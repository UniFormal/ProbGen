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

  private def clearsNodes(
      edge: HeuristicSearchEdge,
      path: List[(Double, Double)]
  ): Boolean = path.zip(path.tail).forall { case ((startX, startY), (endX, endY)) =>
    val dx = endX - startX
    val dy = endY - startY
    positions.forall { case (name, (x, y)) =>
      if (name == edge.from || name == edge.to) true
      else {
        val projection = ((x - startX) * dx + (y - startY) * dy) / (dx * dx + dy * dy)
        val fraction = math.max(0.0, math.min(1.0, projection))
        // The SVG node radius is 0.28 layout units. Leave room around the outline.
        math.hypot(x - startX - fraction * dx, y - startY - fraction * dy) >= 0.36
      }
    }
  }

  private val longEdges = graph.edges.filter { edge =>
    val fromY = positions(edge.from)._2
    val toY = positions(edge.to)._2
    positions.values.exists { case (_, y) => fromY < y && y < toY }
  }
  private val rightmost = positions.values.map(_._1).max
  private val bends = longEdges.zipWithIndex.flatMap { case (edge, index) =>
    val start @ (fromX, fromY) = positions(edge.from)
    val end @ (toX, toY) = positions(edge.to)
    if (clearsNodes(edge, List(start, end))) None
    else {
      val middleX = (fromX + toX) / 2
      val middleY = (fromY + toY) / 2
      val localBend = List(-0.25, 0.25, -0.5, 0.5, -0.75, 0.75)
        .map(offset => (middleX + offset, middleY))
        .find(point => point._1 >= 0 && point._1 <= rightmost &&
          clearsNodes(edge, List(start, point, end)))
      val lane = rightmost + 0.7 + index * 0.45
      val route = localBend.map(List(_)).getOrElse(List(
        (fromX, fromY + 0.45), (lane, fromY + 0.45),
        (lane, toY - 0.45), (toX, toY - 0.45)
      ))
      Some((edge.from, edge.to) -> route)
    }
  }.toMap

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
    directed = true,
    bends = bends
  )
  def toHTML = diagram.toHTML
  override def toString = diagram.toString
}
