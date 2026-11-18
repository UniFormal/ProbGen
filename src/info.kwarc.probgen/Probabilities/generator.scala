package info.kwarc.probgen

/** randomly generates a probability problem */
object AbstractProbabilityProblemGenerator extends ProblemGenerator[AbstractProbabilityProblem] {
  private val domainSizeCombinations = List(
    List(2, 3),
    List(3, 2),
    List(3, 3),
    List(2, 2, 2),
    List(2, 2, 3),
    List(2, 3, 2),
    List(3, 2, 2)
  )

  def make(): AbstractProbabilityProblem = {
    val domainSizes = Generator.choose(domainSizeCombinations)
    AbstractProbabilityProblem(BasicProbability(domainSizes))
  }
}

// ***********************************************

/** randomly generates a probability problem with a concrete distribution */
object ConcreteProbabilityProblemGenerator extends ProblemGenerator[ConcreteProbabilityProblem] {
  private val domainSizeCombinations = List(
    List(2, 3),
    List(3, 2),
    List(3, 3),
    List(2, 2, 2),
    List(2, 2, 3),
    List(2, 3, 2),
    List(3, 2, 2)
  )

  def make(): ConcreteProbabilityProblem = {
    val numVars = Generator.choose(List(2,3))
    val nameOffset = Generator.choose(List(0,12,26-numVars))
    val names = Range(nameOffset,nameOffset+numVars).toList.map(i => VarNames.applyUpper(i))
    val smallDomains = List(DUpto(2),DUpto(3),DBool)
    // if only 2 variables, last domain may be big
    val bigDomains = List(DUpto(4), DUpto(5)) //, CartProd(DBool,DBool), CartProd(DUpto(2),DBool))
    val randVars = names.map {n =>
      val domOptions = if (numVars == 2 && n == names.last) smallDomains:::bigDomains else smallDomains
      val d = Generator.choose(domOptions)
      RandVar(n, d)
    }
    val sig = RandSig(randVars)
    val totalProb = 12
    var totalProbUsed = 0
    var nonZero = 0
    val tailProbs = sig.events.tail.map {e =>
      val zero = totalProbUsed == totalProb || Generator.chooseBoolean(0.2*nonZero) // at most 5 non-zero values
      val p = if (zero) 0 else {nonZero += 1; Generator.chooseInt(1,totalProb-totalProbUsed)}
      totalProbUsed += p
      p
    }
    val probs = (totalProb-totalProbUsed)::tailProbs // ensure sum is totalProb
    val rd = RandDist(sig, probs.map(_/totalProb))
    ConcreteProbabilityProblem(rd)
  }
}
