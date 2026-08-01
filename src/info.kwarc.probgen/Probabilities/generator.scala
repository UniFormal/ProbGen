package info.kwarc.probgen

/** randomly generates a search problem according to some criteria */
object BasicProbabilityProblemGenerator extends ProblemGenerator[BasicProbabilityProblem] {

  // only between 6 and 12 combinations
  // WARNING: adding List(3,3,3) leads to a infinite loop because we enforce that for the conditional
  // probability the condition A ? B  is true at most 6 times, but the minimum possible is 9
  private val domainSizeCombinations = List(
    List(2, 3),
    List(3, 2),
    List(3, 3),
    List(2, 2, 2),
    List(2, 2, 3),
    List(2, 3, 2),
    List(3, 2, 2)
  )

  def make(): BasicProbabilityProblem = {
    val domainSizes = Generator.choose(domainSizeCombinations)
    BasicProbabilityProblem(BasicProbability(domainSizes))
  }
}
