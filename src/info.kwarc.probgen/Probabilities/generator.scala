package info.kwarc.probgen

/** randomly generates a search problem according to some criteria */
object BasicProbabilityProblemGenerator extends ProblemGenerator[BasicProbabilityProblem] {
  def make(): BasicProbabilityProblem = {
    val numVars = Generator.choose(List(2,3))
    val domainSizes = Range(0,numVars).toList.map(_ => Generator.choose(List(2,3)))
    BasicProbabilityProblem(BasicProbability(domainSizes))
  }
}
