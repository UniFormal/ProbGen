package info.kwarc.probgen

/** randomly generates a search problem according to some criteria */
object BasicProbabilityProblemGenerator extends ProblemGenerator[BasicProbabilityProblem] {

  // only between 6 and 12 combinations
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
    val numVars = domainSizes.length
    BasicProbabilityProblem(BasicProbability(domainSizes))
  }
}
