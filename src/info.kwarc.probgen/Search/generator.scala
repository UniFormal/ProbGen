package info.kwarc.probgen

/** randomly generates a search problem according to some criteria */
object SearchProblemGenerator extends ProblemGenerator[ExpressionBasedDeterministicSearchProblem] {

  /** modify these to guide selection
   * be very careful to avoid impossible situations
   * f.e. minStates=4 and minSolutionLength=4 is impossible
   * f.e. minStates=6 and minActions=4 and minSolutionLength=4 is almost impossible
  **/

  // params for random generation
  val minStates = 7
  val maxStates = 10
  val minActions = 3
  val maxActions = 4
  val transitionModuloChance = 0.5

  // params for solution criteria
  val minSolutionLength = 3
  val minActionsInSolution = 2
  val maxSearchDepth = 6
  val maxAmountSolutions = 4

  def make(): ExpressionBasedDeterministicSearchProblem = {

    // random choices based on params above
    val numStates = Generator.chooseInt(minStates, maxStates)
    val numActions = Generator.chooseInt(minActions, maxActions)
    val transitionModulo = Generator.chooseBoolean(transitionModuloChance)

    val stateList = Range(0,numStates).toList
    log(" " + numStates + " " + numActions + " " + transitionModulo)

    // loop until a good transition function is found

    var searchProb: ExpressionBasedDeterministicSearchProblem = null
    var good: Boolean = false
    while (!good) {

      val trans = genTransition(numStates, transitionModulo)

      // pick initial and goal state (currently only one goal state)
      val initialAndGoalStates = Generator.chooseSome(stateList, 2, 2, false)
      val initial = initialAndGoalStates(0)
      val goal = initialAndGoalStates(1)
      val goalForm: Form = Equals(Var("s"), DInt(goal))

      // actions are numbers similar in size to the states
      var possibleActions = stateList
      if(containsExp(trans)){ // avoid 0^0
        possibleActions = possibleActions.filter(_ != 0)
      }
      val actionList = Generator.chooseSome(possibleActions, numActions, numActions, false).sorted
      val actions = FinSet(actionList.map(DInt(_))*)

      searchProb = ExpressionBasedDeterministicSearchProblem(numStates,actionList,trans,initial,goalForm, maxSearchDepth, transitionModulo)

      // solve the resulting search problem and check if we like the solutions
      good = solutionsValid(searchProb.solutions)
    }

    log("chosen problem: " + searchProb)
    searchProb
  }

  // the transition function is of the form (s op a [op' i]) modulo number of states
  // where op, op' are random operators and i is a number
  def genTransition(numStates: Int, transitionModulo: Boolean): Term = {
      val op1 = Generator.choose(List(Exp))
      val term1 = op1(Var("s"), Var("a"))
      val lit = Generator.chooseInt(0, numStates)
      val term2 = if (lit == 0) {
        term1
      } else {
        val op2 = Generator.choose(List(Plus,Minus))
        op2(term1, DInt(lit))
      }
      val trans = if (transitionModulo) Mod(term2,DInt(numStates)) else term2

      trans
  }

  def solutionsValid(solutions: List[Path[Int, Int]]) : Boolean = {

    if (solutions.isEmpty || solutions.length > maxAmountSolutions) {
      return false
    }

    val sol = solutions.head
    if (sol.length < minSolutionLength || sol.actions.distinct.length < minActionsInSolution) {
      return false
    }

    return true
  }

  def containsExp(t: Term): Boolean = t match {
    case Apply(op, args) =>
      op == Exp || args.exists(containsExp)
    case _ =>
      false
  }

}
