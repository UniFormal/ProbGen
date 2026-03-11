package info.kwarc.probgen

/** randomly generates a search problem according to some criteria */
object SearchProblemGenerator extends ProblemGenerator[ExpressionBasedDeterminisiticSearchProblem] {

  // modify these to guide selection
  val minStates = 5
  val maxStates = 8
  val minActions = 3
  val maxActions = 4
  val minSolutionLength = 4
  val maxSolutions = Some(4)
  val minActionsInSolution = 2
  def getTransitionModulo = Generator.chooseBoolean(0.0)
  def getPresentArithmetically = Generator.chooseBoolean(0.5)

  def make(): ExpressionBasedDeterminisiticSearchProblem = {
    // lower/upper bound for number of states
    val numStates = Generator.chooseInt(minStates,maxStates+1)
    val stateList = Range(0,numStates).toList
    val states = FinSet(stateList.map(Lit(_))*)
    log("choosing states: " + states)
    // lower/upper bound for number of actions; actions are numbers similar in size to the states
    val actionList = Generator.chooseSome(stateList, minActions, maxActions, false).sorted
    val actions = FinSet(actionList.map(Lit(_))*)
    log("choosing actions: " + actions)
    // the initial state is some state
    val initial = if (Generator.chooseBoolean(0.5)) 0 else numStates-1
    log("choosing initial state: " + initial)
    // repeat picking goal conditions until a good one is found
    var goal: Form = null
    var good = false
    log("choosing goal condition")
    while (!good) {
      // goal conditions are of the form p(s,i) where p is some predicate and i is a number
      val goalPred = Generator.choose(List(Less,LessEq,Divides,Equals))
      val goalArg = Generator.choose(Range(0,numStates).toList)
      goal = goalPred(Var("s"), Lit(goalArg))
      // compute the set of goal states and check if we like it
      val goalStates = Range(0,numStates).filter(s => Evaluator(goal)(using Context("s" -> s)))
      val numGoalStates = goalStates.length
      log("  " + goal + " --- " + "satisfied by " + goalStates.mkString(","))
      if (numGoalStates == 0) {
        log("    no goal states --- dismiss")
      } else if (numGoalStates.toDouble/numStates > 0.2) {
        log("    too many goal states --- dismiss")
      } else if (goalStates.contains(initial)) {
        log("    initial is goal --- dismiss")
      } else {
        good = true
        if (numGoalStates == 1) {
          // if there is only one goal state, use the equality as the predicate
          goal = Equals(Var("s"), Lit(goalStates.head))
        }
      }
    }
    // loop until a good transition function is found
    log("choosing transition operation")
    val transitionModulo = getTransitionModulo
    var searchProb: ExpressionBasedDeterminisiticSearchProblem = null
    good = false
    var tries = 0 // count attempts and abort if we can't find anything good
    while (!good && tries < 100) {
      tries += 1
      // the transition function is of the form (s op a [op' i]) modulo number of states
      // where op, op' are random operators and i is a number
      val op1 = Generator.choose(List(Plus,Minus,Times,Exp))
      val term1 = op1(Var("s"), Var("a"))
      val lit = Generator.choose(stateList)
      val term2 = if (lit == 0) {
        term1
      } else {
        val op2 = Generator.choose(List(Plus,Minus))
        op2(term1, Lit(lit))
      }
      log("  " + term2)
      val trans = if (transitionModulo) Mod(term2,Lit(numStates)) else term2
      searchProb = ExpressionBasedDeterminisiticSearchProblem(numStates,actionList,trans,initial,goal)
      // solve the resulting search problem and check if we like the solutions
      val solutions = searchProb.solutions
      if (solutions.isEmpty) {
        log("  no solutions --- dismiss")
      } else {
        val sol = solutions.head
        log("  shortest solutions: " + sol)
        if (sol.length < minSolutionLength) {
          log(s"    length under $minSolutionLength --- dismiss")
        } else if (sol.actions.distinct.length < minActionsInSolution) {
          log(s"    uses fewer than $minActionsInSolution actions --- dismiss")
        } else if (maxSolutions.forall(m => solutions.length > m)) {
          log(s"    more than $maxSolutions solutions --- dismiss")
        } else {
          log("  solutions: ")
          solutions.take(5).foreach(s => log("  " + s))
          good = true
        }
      }
    }
    if (!good) {
      // fail-safe: start from scratch if we're stuck
      log("no problem found, trying again")
      return make()
    }
    searchProb.presentArithmetically = transitionModulo && getPresentArithmetically
    log("chosen problem: " + searchProb)
    searchProb
  }
}
