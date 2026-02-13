package info.kwarc.probgen

import scala.util.Random
import info.kwarc.probgen.SText._ 

case class MDPProblem(mdp: MDP[_,_], description: SText, solU: Map[_, Double], startState: Any, mode: String, initialPolicy: Map[_,_] = Map()) extends Problem[MDPProblem] {
  private val mdpAny = mdp.asInstanceOf[MDP[Any,Any]]
  private val policyAny = initialPolicy.asInstanceOf[Map[Any,Any]]

  def round(d: Double): String = {
    val big = BigDecimal(d)
    big.setScale(3, BigDecimal.RoundingMode.HALF_UP).toString.replaceAll("0*$","").replaceAll("\\.$","")
  }

  def intro(): SText = {
    val methodText = if (mode == "PI") "**Policy Iteration**" else "**Value Iteration**"
    
    SText(
      x"Consider the following stochastic environment.",
      description, 
      x" The discount factor is §\gamma = ${mdp.gamma}§.",
      x" We analyze this using $methodText."
    )
  }

  // --- SUBPROBLEMS ---

  // 1. MODELING
  object defineTuple extends Subproblem("model", 3, 5) {
    def question() = x"Define the states §S§ and actions §A§ for this MDP."
    def solution() = {
      val sDesc = if(mdp.isInstanceOf[GridMDP]) {
        val g = mdp.asInstanceOf[GridMDP]
        x"Grid coordinates §\{ (x,y) \mid 0 \le x < ${g.width}, 0 \le y < ${g.height} \}§"
      } else {
        val c = mdp.asInstanceOf[CircularMDP]
        x"States §\{ 0, \dots, ${c.numStates - 1} \}§"
      }
      val actionsList = mdpAny.actions.map(Expr.fromAny)
      val aSet = FinSet(actionsList:_*)
      
      x"States §S§: $sDesc. Actions §A§: $aSet."
    }
  }

  // 2. THEORY (VI)
  object bellmanEqVI extends Subproblem("theory", 2, 3) {
    override def applicable() = mode == "VI"
    def question() = x"State the **Bellman Optimality Equation** for §U(s)§."
    def solution() = x"§§ U(s) = R(s) + \gamma \max_{a \in A} \sum_{s'} P(s' \mid s, a) U(s') §§"
  }

  // 3. THEORY (PI)
  object bellmanEqPI extends Subproblem("theory", 2, 3) {
    override def applicable() = mode == "PI"
    def question() = x"State the **Bellman Expectation Equation** for a fixed policy §\pi§, i.e., for §U^\pi(s)§."
    def solution() = x"§§ U^\pi(s) = R(s) + \gamma \sum_{s'} P(s' \mid s, \pi(s)) U^\pi(s') §§"
  }

  // 4. CALCULATION (VI)
  object calcOneStepVI extends Subproblem("calc", 3, 4) {
    override def applicable() = mode == "VI"
    val s = Generator.choose(mdpAny.states.filterNot(mdpAny.isTerminal))
    def question() = x"Assume current utilities §U(s) = 0§ for all states. Perform one **Value Iteration** step to calculate §U($s)§."
    def solution() = {
      val res = round(mdpAny.reward(s))
      x"With §U=0§, future rewards are 0. Thus: §§ U($s) = R($s) = $res §§"
    }
  }

  // 5. CALCULATION (PI)
  object calcOneStepPI extends Subproblem("calc", 3, 4) {
    override def applicable() = mode == "PI" && initialPolicy.nonEmpty
    val s = Generator.choose(mdpAny.states.filterNot(mdpAny.isTerminal))
    
    def question() = {
      val a = policyAny.getOrElse(s, mdpAny.actions.head)
      x"Consider policy §\pi_0§ where §\pi_0(s) = $a§. Starting with §V(s) = 0§, perform one step of **Policy Evaluation** to find §V_{k+1}($s)§."
    }
    def solution() = {
      val res = round(mdpAny.reward(s))
      x"Since §V_k=0§, the update is just the immediate reward: §§ V_{k+1}($s) = R($s) = $res §§"
    }
  }

  // 6. POLICY IMPROVEMENT / EXTRACTION
  object policyImprovement extends Subproblem("calc", 3, 4) {
    // Applicable in PI (step 2) OR VI (final step)
    override def applicable() = true 
    val s = Generator.choose(mdpAny.states.filterNot(mdpAny.isTerminal))
    
    def question() = x"Suppose we found the optimal values §U^*(s)§. How do we derive the optimal action §\pi^*(s)§ for state §s=$s§?"
    def solution() = {
      x"Calculate Q-values for all actions and pick the max: §§ \pi^*(s) = \text{argmax}_{a} \sum_{s'} P(s' \mid s, a) U^*(s') §§"
    }
  }

  // 7. PATH PROBABILITY
  object sequenceProbability extends Subproblem("calc", 2, 3) {
    val s0 = startState
    val a0 = Generator.choose(mdpAny.actions)
    val dist0 = mdpAny.trans(s0, a0)
    val s1 = dist0.keys.headOption.getOrElse(s0) 
    val a1 = Generator.choose(mdpAny.actions)
    val dist1 = mdpAny.trans(s1, a1)
    val s2 = dist1.keys.headOption.getOrElse(s1)

    def question() = x"Calculate the probability of the path: §§ $s0 \xrightarrow{$a0} $s1 \xrightarrow{$a1} $s2 §§"
    
    def solution() = {
      val p1 = dist0.getOrElse(s1, 0.0)
      val p2 = dist1.getOrElse(s2, 0.0)
      val total = round(p1 * p2)
      x"§§ P = $p1 \times $p2 = $total §§"
    }
  }

  // 8. POMDP BELIEF
  object beliefUpdate extends Subproblem("calc", 3, 4) {
    val sA = startState
    val others = mdpAny.states.filter(_ != sA)
    val sB = if (others.nonEmpty) others.head else sA
    val a = Generator.choose(mdpAny.actions)

    def question() = x"**POMDP**: Current belief is §b($sA)=0.5, b($sB)=0.5§. Action §a=$a§ is applied. Calculate the new belief state §b'(s)§."
    
    def solution() = {
      val distA = mdpAny.trans(sA, a)
      val distB = mdpAny.trans(sB, a)
      val allNext = distA.keys ++ distB.keys
      val newBeliefs = allNext.map { s =>
        val p = 0.5 * distA.getOrElse(s, 0.0) + 0.5 * distB.getOrElse(s, 0.0)
        (s, round(p))
      }.filter(_._2.toDouble > 0.0)

      val resStr = newBeliefs.map{case(k,v) => x"b'($k)=$v"}.mkString(", ")
      x"§§ b'(s) = \{ $resStr \} §§"
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
  GroupConstraint(1, 1, policyImprovement, sequenceProbability, beliefUpdate)
}