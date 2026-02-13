package info.kwarc.probgen

/**
  * A generic Markov Decision Process
  */
trait MDP[S, A] {
  def states: List[S]
  def actions: List[A]
  def trans(s: S, a: A): Map[S, Double]
  def reward(s: S): Double
  def gamma: Double
  def isTerminal(s: S): Boolean

  // --- Value Iteration Tools ---
  
  /** * Runs one step of Bellman Optimality Update (Value Iteration) 
   */
  def valueIterationStep(U: Map[S, Double]): Map[S, Double] = {
    states.map { s =>
      val newVal = if (isTerminal(s)) {
        reward(s)
      } else {
        val bestActionValue = actions.map { a =>
          val transitions = trans(s, a)
          transitions.map { case (nextState, prob) => 
            prob * U.getOrElse(nextState, 0.0) 
          }.sum
        }.max
        reward(s) + gamma * bestActionValue
      }
      s -> newVal
    }.toMap
  }

  def solve(iterations: Int = 100, epsilon: Double = 0.001): Map[S, Double] = {
    var U: Map[S, Double] = states.map(s => s -> 0.0).toMap
    for (i <- 1 to iterations) {
      val U_new = valueIterationStep(U)
      val delta = states.map(s => Math.abs(U_new(s) - U(s))).max
      U = U_new
      if (delta < epsilon) return U
    }
    U
  }

  def getOptimalPolicy(U: Map[S, Double]): Map[S, A] = {
    states.filterNot(isTerminal).map { s =>
      val bestAction = actions.maxBy { a =>
        trans(s, a).map { case (sPrime, p) => p * U.getOrElse(sPrime, 0.0) }.sum
      }
      s -> bestAction
    }.toMap
  }

  // --- Policy Iteration Tools ---

  /** * Runs one step of Iterative Policy Evaluation (Expectation Update) 
   * U_new(s) = R(s) + gamma * sum(P(s'|s, pi(s)) * U(s'))
   */
  def policyEvalStep(U: Map[S, Double], policy: Map[S, A]): Map[S, Double] = {
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
  def policyImprovement(U: Map[S, Double]): Map[S, A] = getOptimalPolicy(U)

  def stepsToGoal(start: S, policy: Map[S, A]): Int = {
    var current = start
    var steps = 0
    var visited = Set[S](start)
    while (!isTerminal(current) && steps < 20) {
      if (!policy.contains(current)) return 999 
      val a = policy(current)
      val next = trans(current, a).maxBy(_._2)._1
      if (visited.contains(next)) return 999 
      visited += next
      current = next
      steps += 1
    }
    steps
  }
}

// --- Concrete Implementations ---

case class CircularMDP(numStates: Int, successProb: Double, riskActionAllowed: Boolean) extends MDP[Int, Int] {
  val states = Range(0, numStates).toList
  val actions = if (riskActionAllowed) List(1, -1, 2) else List(1, -1)
  val gamma = 0.5 
  val goal = 0    

  def isTerminal(s: Int) = s == goal
  def reward(s: Int) = if (s == goal) 10.0 else -0.1

  def trans(s: Int, a: Int): Map[Int, Double] = {
    if (isTerminal(s)) return Map(s -> 1.0) 
    
    // Normalize states to be within [0, numStates-1]
    def norm(x: Int) = (x % numStates + numStates) % numStates
    
    val forward = norm(s + a)
    val backward = norm(s - 1)
    val stay = s

    if (Math.abs(a) == 1) {
       Map(forward -> successProb, stay -> (1.0 - successProb))
    } else {
       Map(forward -> successProb, backward -> (1.0 - successProb))
    }
  }
}

case class GridMDP(width: Int, height: Int) extends MDP[(Int,Int), String] {
  val states = (for (x <- 0 until width; y <- 0 until height) yield (x, y)).toList
  val actions = List("Up", "Down", "Left", "Right")
  val gamma = 0.5
  val goal = (width - 1, height - 1)
  
  def isTerminal(s: (Int,Int)) = s == goal
  // Reward: -2.0 for non-goals ensures nice numbers with gamma=0.5
  def reward(s: (Int,Int)) = if (s == goal) 10.0 else -2.0 

  def trans(s: (Int,Int), a: String): Map[(Int,Int), Double] = {
    if (isTerminal(s)) return Map(s -> 1.0)
    val (x,y) = s
    val intended = a match {
      case "Up"    => (x, math.min(y + 1, height - 1))
      case "Down"  => (x, math.max(y - 1, 0))
      case "Right" => (math.min(x + 1, width - 1), y)
      case "Left"  => (math.max(x - 1, 0), y)
    }
    if (intended == s) Map(s -> 1.0)
    else Map(intended -> 0.8, s -> 0.2)
  }
}