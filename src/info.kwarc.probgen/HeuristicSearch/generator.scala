package info.kwarc.probgen

object HeuristicSearchProblemGenerator extends ProblemGenerator[HeuristicSearchProblem] {
  def make(): HeuristicSearchProblem = HeuristicSearchProblem()
}
