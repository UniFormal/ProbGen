package info.kwarc.probgen

import SText._

/**
  * A generic Markov Decision Process
  */
trait MDP {
  type S
  type A
  type Utilities = Map[S, Double]
  type BeliefState = Map[S, Double]
  type Policy = Map[S,A]

  def states: List[S]
  def actions: List[A]
  def trans(s: S, a: A): BeliefState
  def reward(s: S): Double
  def gamma: Double
  def isTerminal(s: S): Boolean
  val initial: S
  val goal: S
  var utilities: Utilities = null

  def description(): STeXSyntax

  // --- Value Iteration Tools ---
  
  /**
    * Runs one step of Bellman Optimality Update (Value Iteration), no state change
    */
  def valueIterationStep(U: Utilities): Utilities = {
    states.map {s =>
      val newVal = if (isTerminal(s)) {
        reward(s)
      } else {
        val bestActionValue = actions.map {a =>
          val transitions = trans(s, a)
          transitions.map {
            case (nextState, prob) => prob * U.getOrElse(nextState, 0.0)
          }.sum
        }.max
        reward(s) + gamma * bestActionValue
      }
      s -> newVal
    }.toMap
  }

  /** computes utilities and stores them */
  def solve(iterations: Int = 100, epsilon: Double = 0.001): Utilities = {
    var U: Utilities = states.map(s => s -> 0.0).toMap
    for (i <- 1.to(iterations)) {
      val U_new = valueIterationStep(U)
      val delta = states.map(s => Math.abs(U_new(s) - U(s))).max
      U = U_new
      if (delta < epsilon) {utilities = U; return U}
    }
    utilities = U
    U
  }

  /** extracts optimal policy from utilities (solves if needed) */
  def getOptimalPolicy(): Policy = {
    if (utilities == null) solve()
    states.filterNot(isTerminal).map {s =>
      val bestAction = actions.maxBy {a =>
        trans(s, a).map {case (sPrime, p) => p * utilities.getOrElse(sPrime, 0.0)}.sum
      }
      s -> bestAction
    }.toMap
  }

  // --- Policy Iteration Tools ---

  /** * Runs one step of Iterative Policy Evaluation (Expectation Update) 
   * U_new(s) = R(s) + gamma * sum(P(s'|s, pi(s)) * U(s'))
   */
  def policyEvalStep(U: Utilities, policy: Policy): Utilities = {
    states.map { s =>
      val newVal = if (isTerminal(s)) {
        reward(s)
      } else {
        // If policy doesn't define action for s (e.g. goal), assume 0 value or stay
        if (!policy.contains(s)) reward(s) 
        else {
          val a = policy(s)
          val transitions = trans(s, a)
          val expectedFuture = transitions.map { case (ns, p) => p * U.getOrElse(ns, 0.0) }.sum
          reward(s) + gamma * expectedFuture
        }
      }
      s -> newVal
    }.toMap
  }

  /**
   * Improves policy based on current Values (Greedy)
   */
  // def policyImprovement(): Policy = getOptimalPolicy(utilities)

  def stepsToGoal(start: S, policy: Policy): Int = {
    var current = start
    var steps = 0
    var visited = Set[S](start)
    while (!isTerminal(current) && steps < 20) {
      if (!policy.contains(current)) return 999
      val a = policy(current)
      val next = trans(current,a).maxBy(_._2)._1
      if (visited.contains(next)) return 999
      visited += next
      current = next
      steps += 1
    }
    steps
  }
}

// --- Concrete Implementations ---

case class CircularMDP(numStates: Int, initial: Int, successProb: Double) extends MDP {
  type S = Int
  type A = Int

  val states = Range(0, numStates).toList
  val actions = List(1, -1, 2)
  val gamma = 0.5
  val goal = 0

  def description() = SSnippet(List(
    x"The agent moves on a cyclic track of $numStates states (${0} to ${numStates-1}).",
    x"Its possible actions are:",
    SItemize(
      x"${-1} (move left) or ${1} (move right), which make no movement if they fail",
      x"${2} (move left twice), which moves one step to the right if it fails"
    ),
    x"All actions succeed with probability $successProb and fail otherwise."
  ), "\n")
  def isTerminal(s: Int) = s == goal
  def reward(s: Int) = if (s == goal) 10.0 else -0.1

  def trans(s: Int, a: Int): BeliefState = {
    if (isTerminal(s)) return Map(s -> 1.0)
    // x modulo numStates yielding [0, numStates-1]
    def norm(x: Int) = {val r = x % numStates; if (r < 0) r + numStates else r}
    val intended = norm(s + a)
    val fail = norm(if (a == 2) s - 1 else s - a)
    Map(intended -> successProb,fail -> (1.0 - successProb))
  }
}

case class GridMDP(width: Int, height: Int, successProb: Double) extends MDP {
  type S = (Int,Int)
  type A = (Int,Int)

  val states = (for (x <- 0.until(width); y <- 0.until(height)) yield (x, y)).toList
  val namedActions = List(("up",(0,1)),("down",(0,-1)),("right",(1,0)),("left",(-1,0)))
  val actions = namedActions.map(_._2)
  val gamma = 0.5
  val initial = (0,0)
  val goal = (width-1, height-1)

  def description() = SSnippet(List(
    x"The agent moves in a §$width \times $height§ grid.",
    x"Its possible actions are ${namedActions.map(a => x"§\mathtt{${a._1}}§").mkString(", ")}.",
    x"These succeed with a probability of $successProb and fail without movement otherwise.",
    x""
  ),"\n")

  def isTerminal(s: S) = s == goal
  // Reward: -2.0 for non-goals ensures nice numbers with gamma=0.5
  def reward(s: S) = if (s == goal) 10.0 else -2.0

  def trans(s: S, a: A): BeliefState = {
    // bound to [0,...,l]
    def norm(x: Int, l: Int) = if (x<0) 0 else if (x >= l) l-1 else x
    if (isTerminal(s)) return Map(s -> 1.0)
    val intended = (norm(s._1+a._1, width), norm(s._2+a._2, height))
    if (intended == s) Map(s -> 1.0)
    else Map(intended -> successProb, s -> (1.0-successProb))
  }
}