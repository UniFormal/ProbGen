package info.kwarc.probgen

import SText._

case class HeuristicSearchProblem() extends Problem[HeuristicSearchProblem] {
  def intro() = x"This is a placeholder for a heuristic search exercise."

  object placeholder extends Subproblem("heuristic-search-placeholder", 1, 1) {
    override def answerHint = "a number"
    override def grader = Some(NumberGrader(1, tol = 0))
    def question() = x"Enter the number 1."
    def solution() = x"1"
  }

  GroupConstraint(1, 1, placeholder)
}
