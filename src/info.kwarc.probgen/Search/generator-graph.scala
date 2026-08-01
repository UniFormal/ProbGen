package info.kwarc.probgen

/** randomly generates a search problem according to some criteria */
object SearchProblemGenerator2 extends ProblemGenerator[ExpressionBasedDeterministicSearchProblem] {

  // modify these to guide selection
  val numStates = Generator.chooseInt(6, 10)
  val numActions = Generator.chooseInt(3, 4)
  val minSolutionLength = 4
  val maxSolutions = Some(4)
  val minActionsInSolution = 2

  def make(): ExpressionBasedDeterministicSearchProblem = {

    val stateList = Range(0, numStates).toList
    val initialAndGoalStates = Generator.chooseSome(stateList, 2, 2, false)
    val initial = initialAndGoalStates(1)
    val goal = initialAndGoalStates(2)

    while(true) {
      val adj = genAdj()

      

    }

    null
  }

  // randomly generate nxn matrix of 0s and 1s with at most numAct
  def genAdj(): List[List[Int]] = {
    val maxOnesPerRow = math.min(numActions, numStates)

    Range(0, numStates).toList.map { _ =>
      val cols = Range(0, numStates).toList
      val ones = Generator.chooseSome(cols, 0, maxOnesPerRow, false)
      cols.map(c => if (ones.contains(c)) 1 else 0)
    }
  }

  def findSolution(adj: List[List[Int]]) = {
    0
  }

}
