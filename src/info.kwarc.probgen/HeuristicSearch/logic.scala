package info.kwarc.probgen

/** A directed search graph, independent of its drawing. */
case class HeuristicSearchNode(name: String, heuristic: Int)
case class HeuristicSearchEdge(from: String, to: String, cost: Int)
case class HeuristicSearchGraph(nodes: List[HeuristicSearchNode], edges: List[HeuristicSearchEdge], root: String)
