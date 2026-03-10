package info.kwarc.probgen

import SText._

case class MDPProblem(mdp: MDP, isPolIter: Boolean) extends Problem[MDPProblem] {
  def round(d: Double): String = {
    val big = BigDecimal(d)
    big.setScale(3, BigDecimal.RoundingMode.HALF_UP).toString.replaceAll("0*$","").replaceAll("\\.$","")
  }

  def intro(): SText = {
    val methodText = if (isPolIter) "policy iteration" else "value iteration"
    
    SSnippet(List(
      x"Consider the following agent in a stochastic environment.",
      mdp.description(),
      x"The agent starts at ${mdp.initial}. Its goal is to reach ${mdp.goal}.",
      x"We use a reward of ${mdp.goalReward} for the goal and ${mdp.nongoalReward} otherwise, and a discount factor of §\gamma = ${mdp.gamma}§.",
      x"We analyze this using $methodText."
    ), "\n")
  }

  // --- SUBPROBLEMS ---

  // 1. MODELING
  object defineTuple extends Subproblem("model", 3, 5) {
    def question() = x"Give the sets of states §S§ and actions §A§ for this MDP."
    def solution() = {
      val actions = mdp.actions.map(mdp.actionName)
      x"§S=${Set(mdp.states*)}§, §A=${Set(actions*)}§"
    }
  }

  // 2. THEORY (VI)
  object bellmanEqVI extends Subproblem("theory", 2, 3) {
    override def applicable() = !isPolIter
    def question() = x"State the Bellman Optimality Equation for §U(s)§."
    def solution() = x"\[ U(s) = R(s) + \gamma \max_{a \in A} \sum_{s'} P(s' \mid s, a) U(s') \]"
  }

  // 3. THEORY (PI)
  object bellmanEqPI extends Subproblem("theory", 2, 3) {
    override def applicable() = isPolIter
    def question() = x"State the Bellman Expectation Equation for a fixed policy §\pi§, i.e., for §U^\pi(s)§."
    def solution() = x"\[ U^\pi(s) = R(s) + \gamma \sum_{s'} P(s' \mid s, \pi(s)) U^\pi(s') \]"
  }

  // 4. CALCULATION (VI)
  object calcOneStepVI extends Subproblem("calc", 3, 4) {
    override def applicable() = !isPolIter
    val s = Generator.choose(mdp.states.filterNot(mdp.isTerminal))
    def question() = x"Assume current utilities §U(s) = 0§ for all states. Perform one value iteration step to calculate §U($s)§."
    def solution() = {
      val res = round(mdp.reward(s))
      x"With §U=0§, future rewards are 0. Thus: \[ U($s) = R($s) = $res \]"
    }
  }

  // 5. CALCULATION (PI)
  object calcOneStepPI extends Subproblem("calc", 3, 4) {
    override def applicable() = isPolIter
    val s = Generator.choose(mdp.states.filterNot(mdp.isTerminal))

    def question() = {
      val a = mdp.actionName(mdp.actions.head)
      x"Consider policy §\pi_0§ where §\pi_0(s) = $a§. Starting with §V_k(s) = 0§, perform one step of policy evaluation to find §V_{k+1}($s)§."
    }
    def solution() = {
      val res = round(mdp.reward(s))
      x"Since §V_k(s)=0§, the update is just the immediate reward: \[ V_{k+1}($s) = R($s) = $res \]"
    }
  }

  // 6. POLICY IMPROVEMENT / EXTRACTION
  object policyImprovement extends Subproblem("calc", 3, 4) {
    // Applicable in PI (step 2) OR VI (final step)
    override def applicable() = true 
    val s = Generator.choose(mdp.states.filterNot(mdp.isTerminal))
    
    def question() = x"Suppose we solved the utilities as §U^*(s)§. How do we derive the optimal action §\pi^*(s)§ for state §s=$s§?"
    def solution() = {
      x"Calculate utilities for all actions and pick the max: \[ \pi^*(s) = \text{argmax}_{a} \sum_{s'} P(s' \mid s, a) U^*(s') \]"
    }
  }

  // 7. PATH PROBABILITY
  object sequenceProbability extends Subproblem("calc", 2, 3) {
    val s0 = mdp.initial
    val a0 = Generator.choose(mdp.actions)
    val dist0 = mdp.trans(s0, a0)
    val s1 = dist0.keys.headOption.getOrElse(s0) 
    val a1 = Generator.choose(mdp.actions)
    val dist1 = mdp.trans(s1, a1)
    val s2 = dist1.keys.headOption.getOrElse(s1)

    def question() = x"Calculate the probability of the agent moving along §$s0\to $s1\to$s2§"
    + x"by taking the actions §${mdp.actionName(a0)}§ and §${mdp.actionName(a1)}§."
    
    def solution() = {
      val p1 = dist0.getOrElse(s1, 0.0)
      val p2 = dist1.getOrElse(s2, 0.0)
      val total = round(p1 * p2)
      x"\[ P = $p1 \times $p2 = $total \]"
    }
  }

  // 8. POMDP belief
  object beliefUpdate extends Subproblem("calc", 3, 4) {
    val sA = mdp.initial
    val others = mdp.states.filter(_ != sA)
    val sB = if (others.nonEmpty) others.head else sA
    val a = Generator.choose(mdp.actions)

    def question() = x"Now assume we use the corresponding POMDP with current belief state §b($sA)=0.5, b($sB)=0.5§ and apply action §a=$a§." +
      x" Calculate the new belief state §b'§."
    
    def solution() = {
      val distA = mdp.trans(sA, a)
      val distB = mdp.trans(sB, a)
      val allNext = distA.keys ++ distB.keys
      val newBeliefs = allNext.map { s =>
        val p = 0.5 * distA.getOrElse(s, 0.0) + 0.5 * distB.getOrElse(s, 0.0)
        (s, round(p))
      }.filter(_._2.toDouble > 0.0)
      SSnippet(newBeliefs.map{case(k,v) => x"§b'($k)=$v§"}.toList, ", ")
    }
  }
  
  // --- CONSTRAINTS ---
  // 1. Always define the tuple (Modeling)
  GroupConstraint(1, 1, defineTuple)
  
  // 2. Pick the correct Bellman Equation (Theory)
  GroupConstraint(1, 1, bellmanEqVI, bellmanEqPI)
  
  // 3. Pick the correct One-Step Calculation (Algorithm)
  GroupConstraint(1, 1, calcOneStepVI, calcOneStepPI)
  
  // 4. Pick 1 Advanced Calculation (Policy logic, Path logic, or POMDPs)
  GroupConstraint(3, 3, policyImprovement, sequenceProbability, beliefUpdate)
}