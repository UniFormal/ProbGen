package info.kwarc.probgen

/** the notation questions have no shared instance; each question makes its own random choices */
object NotationProblemGenerator extends ProblemGenerator[NotationProblem] {
  def make(): NotationProblem = NotationProblem()
}
