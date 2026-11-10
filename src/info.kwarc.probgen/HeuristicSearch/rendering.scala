package info.kwarc.probgen

/** Uses the shared top-down layout for the fixed example tree. */
case class SHeuristicSearchTree(graph: HeuristicSearchGraph) extends SText {
  private val byName = graph.nodes.map(n => n.name -> n).toMap
  private val diagram = STreeDiagram[HeuristicSearchNode](
    byName(graph.root),
    _.name,
    n => graph.edges.filter(_.from == n.name).map(e => byName(e.to)),
    _ => None,
    n => Some(n.heuristic),
    (from, to) => graph.edges.find(e => e.from == from && e.to == to).map(_.cost),
    directed = true
  )
  def toHTML = diagram.toHTML
  override def toString = diagram.toString
}
