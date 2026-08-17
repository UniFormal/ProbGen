package info.kwarc.probgen

/** randomly generates a search problem according to some criteria */
object SearchProblemGenerator
    extends ProblemGenerator[ExpressionBasedDeterministicSearchProblem] {

  /** modify these to guide selection be very careful to avoid impossible
    * situations like: f.e. minStates=4 and minSolutionLength=4 f.e. minStates=6
    * and minActions=4 and minSolutionLength=4 with transitionModuloChance>0
    */

  // params for random generation, used once
  val minStates = 7
  val maxStates = 10
  val minActions = 2
  val maxActions = 3
  val transitionModuloChance = 2d / 3d

  // params for solution criteria
  val minSolutionLength = 3
  val minActionsInSolution = 2
  val maxSearchDepth = 6
  val maxAmountSolutions = 4

  // param that only influences visual representation and only if transitionModulo=true
  val presentArithmeticallyChance = 0.5

  def make(): ExpressionBasedDeterministicSearchProblem = {
    // testAllCombinationsValid() // uncomment if you want to test it
    // random choices based on params above
    val params = generateMainParameters()
    val searchProb = findGoodProblem(params)
    log("chosen problem: " + searchProb)
    searchProb
  }

  def generateMainParameters(): (Int, Int, Boolean) = {
    val ns = Generator.chooseInt(minStates, maxStates)
    val na = Generator.chooseInt(minActions, maxActions)
    val tm = Generator.chooseBoolean(transitionModuloChance)
    (ns, na, tm)
  }

  def findGoodProblem(params: (Int, Int, Boolean)): ExpressionBasedDeterministicSearchProblem = {
    val numStates: Int = params(0)
    val numActions = params(1)
    val transitionModulo = params(2)
    val stateList = Range(0, numStates).toList
    val presentArithmetically = transitionModulo && Generator.chooseBoolean(presentArithmeticallyChance)

    var searchProb: ExpressionBasedDeterministicSearchProblem = null
    var good: Boolean = false
    while (!good) {
      // pick initial and goal state (currently only one goal state)
      val initialAndGoalStates = Generator.chooseSome(stateList, 2, 2, false)
      val initial = initialAndGoalStates(0)
      val goal = initialAndGoalStates(1)
      val goalForm: Form = Equals(Var("s"), DInt(goal))
      // actions are numbers similar in size to the states
      var possibleActions = stateList
      val trans = genTransition(numStates, transitionModulo)
      if (containsExp(trans)) { // avoid 0^0
        possibleActions = possibleActions.filter(_ != 0)
      }
      val actionList = Generator.chooseSome(possibleActions, numActions, numActions, false).sorted
      searchProb = ExpressionBasedDeterministicSearchProblem(numStates,actionList,trans,initial,goalForm,maxSearchDepth,presentArithmetically)
      // solve the resulting search problem and check if we like the solutions
      good = solutionsValid(searchProb.solutions)
    }
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
      val op2 = Generator.choose(List(Plus, Minus))
      op2(term1, DInt(lit))
    }
    val trans = if (transitionModulo) Mod(term2, DInt(numStates)) else term2
    trans
  }

  def solutionsValid(solutions: List[Path[Int, Int]]): Boolean = {
    if (solutions.isEmpty || solutions.length > maxAmountSolutions) {
      return false
    }
    val sol = solutions.head
    if (sol.length < minSolutionLength || sol.actions.distinct.length < minActionsInSolution) {
      return false
    }
    true
  }

  def containsExp(t: Term): Boolean = t match {
    case Apply(op, args) =>
      op == Exp || args.exists(containsExp)
    case _ =>
      false
  }

  // functions for testing, if this does not finish some combination is impossible
  def testAllCombinationsValid() = {
    val repetitions = 10
    for (combination <- generateAll()) {
      val start = System.nanoTime()
      for (_ <- Range(0, repetitions)) {
        findGoodProblem(combination)
      }
      val elapsedTimeMs = (System.nanoTime() - start) / 1_000_000.0
      log(
        "combination" + combination + " took on average " + elapsedTimeMs / repetitions + "ms"
      )
    }
  }

  def generateAll(): List[(Int, Int, Boolean)] = {
    var options: List[(Int, Int, Boolean)] = List()
    for (ns <- Range(minStates, maxStates + 1)) {
      for (na <- Range(minActions, maxActions + 1)) {
        for (tm <- List(true, false)) {
          options = options :+ (ns, na, tm)
        }
      }
    }
    options
  }
}
