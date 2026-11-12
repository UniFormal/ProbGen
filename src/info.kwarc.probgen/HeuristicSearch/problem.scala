package info.kwarc.probgen

import SText._

case class HeuristicSearchProblem(graph: HeuristicSearchGraph, positions: Map[String, (Double, Double)]) extends Problem[HeuristicSearchProblem] {
  def intro() = SSnippet(List(
    if (graph.nodes.forall(_.heuristic.isDefined) && graph.edges.forall(_.cost.isDefined))
      x"Each node shows its name and heuristic value. Each directed edge is labeled with its cost."
    else x"Follow the arrows to see which nodes are reachable from A.",
    SHeuristicSearchGraph(graph, positions)
  ), "\n")

  object placeholder extends Subproblem("heuristic-search-placeholder", 1, 1) {
    override def answerHint = "a number"
    override def grader = Some(NumberGrader(1, tol = 0))
    def question() = x"Enter the number 1."
    def solution() = x"1"
  }

  GroupConstraint(1, 1, placeholder)
}
