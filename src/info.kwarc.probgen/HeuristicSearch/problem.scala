package info.kwarc.probgen

import SText._

case class HeuristicSearchProblem(graph: HeuristicSearchGraph, positions: Map[String, (Double, Double)]) extends Problem[HeuristicSearchProblem] {
  def intro() = SSnippet(List(
    x"Each node shows its name and heuristic value. Each directed edge is labeled with its cost.",
    SHeuristicSearchGraph(graph, positions),
    x"Assume you have already expanded the root node A. In each question, list the next 4 nodes expanded by the respective algorithm. Break ties alphabetically and visit depth-first children alphabetically. Expand each node at most once, skipping any later occurrence. Continue listing expansions even if a goal node is reached."
  ), "\n")

  class ExpansionQuestion(val algorithm: HeuristicSearchAlgorithm)
      extends Subproblem(algorithm.toString, algorithm.points, 2) {
    lazy val expected: List[String] = HeuristicSearch.nextExpansions(graph, algorithm)
    override def answerHint = "four nodes in order, e.g. B, C, D, E"
    def question() = x"${algorithm.title}"
    def solution() = SText(expected.mkString(", "))

    override def checkSolution(input: String): CheckResult = {
      val parsed = ExprParser.parseExprList(input).flatMap { terms =>
        Grader.sequence(terms) {
          case Var(name) if graph.nodes.exists(_.name == name.toUpperCase) => Right(name.toUpperCase)
          case _ => Left("Enter node names in order, separated by commas, e.g. B, C, D, E.")
        }
      }
      Grader.onParsed(parsed) { answer =>
        if (answer == expected) Correct()
        else Incorrect("Check the expansion order. Start after A, break ties alphabetically, and expand each node only once.")
      }
    }
  }

  val questions = HeuristicSearchAlgorithm.values.toList.map(new ExpansionQuestion(_))
  questions.foreach(question => GroupConstraint(1, 1, question))
}
