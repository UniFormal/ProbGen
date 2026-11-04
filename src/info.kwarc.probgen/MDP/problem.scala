package info.kwarc.probgen

import SText._
import Expr._
import scala.compiletime.uninitialized

case class MDPProblem(mdp: MDP,isPolIter: Boolean) extends Problem[MDPProblem] {
  def round(d: Double): String = {
    val big = BigDecimal(d)
    big.setScale(3, BigDecimal.RoundingMode.HALF_UP).toString.replaceAll("0*$","").replaceAll("\\.$","")
  }

  val U = Var("U")
  val R = Var("R")

  // ── For grading ──────────────────────────────────────────────────────────

  /** a state or an action as a term: a number, a grid cell (x, y), or an action name */
  private def term(x: Any): Term = Expr.fromAny(x)

  /** a state the way it is typed inside b'(…): 3, or 0, 1 for the grid cell (0, 1) */
  def typedInside(s: mdp.S): String = term(s) match {
    case Tuple(args) => args.map(ExprCompare.show).mkString(", ")
    case t           => ExprCompare.show(t)
  }

  /** reads b'(0, 1) as b'((0, 1)) */
  private def cellArgument(t: Term): Term = t match {
    case FunApply(Seq(f, a, b, more*)) => FunApply(f, Tuple((a :: b :: more.toList)*))
    case other                         => other
  }

  /** b'(s') = 0.5 P(s'|sA,a) + 0.5 P(s'|sB,a), for every s' with a positive value */
  def newBelief(sA: mdp.S, sB: mdp.S, a: mdp.A): List[(mdp.S, Double)] = {
    val distA = mdp.trans(sA, a)
    val distB = mdp.trans(sB, a)
    // distinct: a state reachable from both sA and sB must appear only once
    (distA.keys ++ distB.keys).toList.distinct.map { s =>
      s -> (0.5 * distA.getOrElse(s, 0.0) + 0.5 * distB.getOrElse(s, 0.0))
    }.filter(_._2 > 0.0)
  }

  def intro(): SText = {
    val methodText = if (isPolIter) "policy iteration" else "value iteration"
    
    SSnippet(List(
      x"Consider the following agent in a stochastic environment.",
      mdp.description(),
      x"The agent starts at ${mdp.initial}. Its goal is to reach ${mdp.goal}.",
      x"We use a reward of ${mdp.goalReward} for the goal and ${mdp.nongoalReward} otherwise, and a discount factor of ${"\\gamma" === mdp.gamma}.",
      x"We analyze this using $methodText."
    ), "\n")
  }

  // --- SUBPROBLEMS ---

  // 1. MODELING
  object defineTuple extends Subproblem("model", 3, 5) {
    override def answerHint =
      s"e.g. S = {${ExprCompare.show(term(mdp.states.head))}, …}, A = {${ExprCompare.show(term(mdp.actionName(mdp.actions.head)))}, …}"
    override def grader = Some(EquationsGrader(List(
      Var("S") -> FinSet(mdp.states.map(term)*),
      Var("A") -> FinSet(mdp.actions.map(a => term(mdp.actionName(a)))*))))
    def question() = x"Give the sets of states ${Var("S")} and actions ${Var("A")} for this MDP."
    def solution() = {
      val actions = mdp.actions.map(mdp.actionName)
      x"${"S" === FinSet(!mdp.states)}, ${"A" === FinSet(!actions)}"
    }
  }

  // 2. THEORY (VI)
  object bellmanEqVI extends Subproblem("theory", 2, 3) {
    override def applicable() = !isPolIter
    override def answerHint = "write the equation, e.g. U(s) = …"
    override def rubric = Some(AnswerRubric(List(
      AnswerClass(1, "U(s) is the reward R(s) plus the discounted (γ ·) utility of what follows."),
      AnswerClass(1, "What follows is the maximum over all actions a of the expected utility Σ_{s'} P(s'|s,a) · U(s').")),
      "Equivalent notation is fine, e.g. T(s,a,s') for P(s'|s,a)."))
    def question() = x"State the Bellman Optimality Equation for ${U("s")}."
    def solution() = SMath(U("s") ===
      R("s") + "\\gamma" * BigMax("a","A")(
        Sum("s'","S")(
          Prob(List("s'"), List("s", "a")) * U("s'")
        )
      )
    )
  }

  // 3. THEORY (PI)
  object bellmanEqPI extends Subproblem("theory", 2, 3) {
    override def applicable() = isPolIter
    override def answerHint = "write the equation, e.g. U^π(s) = …"
    override def rubric = Some(AnswerRubric(List(
      AnswerClass(1, "U^π(s) is the reward R(s) plus the discounted (γ ·) utility of what follows."),
      AnswerClass(1, "What follows is the expected utility Σ_{s'} P(s'|s,π(s)) · U^π(s') for the action π(s) the policy chooses (no maximum over actions).")),
      "Equivalent notation is fine, e.g. T(s,π(s),s') for P(s'|s,π(s))."))
    def question() = x"State the Bellman Expectation Equation for a fixed policy ${Var("\\pi")}, i.e., for ${U("\\pi","s")}."
    def solution() = SMath(U("\\pi", "s") ===
       R("s") + "\\gamma" * Sum("s'", "S")(
         Prob(List("s'"), List("s", "\\pi"("s"))) * U("\\pi","s'")
       )
    )
  }

  // 4. CALCULATION (VI)
  object calcOneStepVI extends Subproblem("calc", 3, 4) {
    override def applicable() = !isPolIter
    var s: mdp.S = uninitialized
    override def init() = {
      s = Generator.choose(mdp.states.filterNot(mdp.isTerminal))
    }
    override def answerHint = "a number, e.g. 3.5"
    override def grader = Some(NumberGrader(mdp.reward(s)))
    def question() = x"Assume current utilities ${U("s") === 0} for all states. Perform one value iteration step to calculate ${U(!s)}."
    def solution() = {
      val res = round(mdp.reward(s))
      x"With ${U===0}, future rewards are ${0}. Thus: ${U("s") === R("s") === res}"
    }
  }

  // 5. CALCULATION (PI)
  object calcOneStepPI extends Subproblem("calc", 3, 4) {
    override def applicable() = isPolIter
    var s: mdp.S = uninitialized
    override def init() = {
      s = Generator.choose(mdp.states.filterNot(mdp.isTerminal))
    }

    override def answerHint = "a number, e.g. 3.5"
    override def grader = Some(NumberGrader(mdp.reward(s)))

    def question() = {
      val a = mdp.actionName(mdp.actions.head)
      x"Consider policy ${Var("\\pi")} where ${"\\pi"("s") === DString(a)}. Starting with ${"V"("s") === 0}, perform one step of policy evaluation to calculate ${"V"(!s)}."
    }
    def solution() = {
      val res = round(mdp.reward(s))
      x"Because the initial utilitis are ${0}, the update is just the reward: ${"V"("s") === R("s") === res}"
    }
  }

  // 6. POLICY IMPROVEMENT / EXTRACTION
  object policyImprovement extends Subproblem("calc", 3, 4) {
    // Applicable in PI (step 2) OR VI (final step)
    override def applicable() = true

    override def answerHint = "in words or as a formula"
    override def rubric = Some(AnswerRubric.of(
      AnswerClass(1, "For each action a, compute its expected utility Σ_{s'} P(s'|s,a) · U(s') from the transition probabilities and U."),
      AnswerClass(1, "Choose the action with the largest expected utility: π(s) = argmax over actions a."),
      AnswerClass(1, "Do this for every state s (the policy chooses one action per state).")))

    def question() = x"Suppose we solved the utilities as ${U("s")} for every state ${Var("s")}. How do we derive the optimal policy?"
    def solution() = {
      x"For every state, calculate the expected utility of each action and pick the best action: ${"\\pi"("s") === BigArgMax("a","A")(Sum("s'","S")(Prob(Seq("s'"),Seq("s", "a")) * U("s'")))}."
    }
  }

  // 7. PATH PROBABILITY
  object sequenceProbability extends Subproblem("calc", 2, 3) {
    val s0 = mdp.initial
    var a0: mdp.A = uninitialized
    var dist0: mdp.BeliefState = Map.empty
    var s1: mdp.S = uninitialized
    var a1: mdp.A = uninitialized
    var dist1: mdp.BeliefState = Map.empty
    var s2: mdp.S = uninitialized
    override def init() = {
      a0 = Generator.choose(mdp.actions)
      dist0 = mdp.trans(s0, a0)
      s1 = dist0.keys.headOption.getOrElse(s0)
      a1 = Generator.choose(mdp.actions)
      dist1 = mdp.trans(s1, a1)
      s2 = dist1.keys.headOption.getOrElse(s1)
    }

    override def answerHint = "a number, e.g. 0.42 or 0.6*0.7"
    override def grader = Some(NumberGrader(dist0.getOrElse(s1, 0.0) * dist1.getOrElse(s2, 0.0)))

    def question() = x"Calculate the probability of the agent moving along to $s1 and then to $s2 "
    + x"by taking the actions ${Var(mdp.actionName(a0))} and ${Var(mdp.actionName(a1))}."

    def solution() = {
      val p1 = dist0.getOrElse(s1, 0.0)
      val p2 = dist1.getOrElse(s2, 0.0)
      // rounded, because e.g. 1.0 - 0.7 is 0.30000000000000004 in floating point
      def lit(d: Double) = DDouble(round(d).toDouble)
      x"${Var("P") === lit(p1) * lit(p2) === lit(p1 * p2)}"
    }
  }

  // 8. POMDP belief
  object beliefUpdate extends Subproblem("calc", 3, 4) {
    val sA = mdp.initial
    val others = mdp.states.filter(_ != sA)
    val sB = if (others.nonEmpty) others.head else sA
    var a: mdp.A = uninitialized
    override def init() = {
      a = Generator.choose(mdp.actions)
    }

    override def answerHint = s"e.g. b'(${typedInside(sA)}) = 0.3, b'(${typedInside(sB)}) = 0.7"
    override def grader = Some(EquationsGrader(
      newBelief(sA, sB, a).map { case (s, p) => (FunApply(Var("b'"), term(s)): Term) -> DDouble(p) },
      omitZeros = true, label = cellArgument))

    def question() = x"Now assume we use the corresponding POMDP with current belief state ${"b"(!sA) === 0.5}, ${"b"(!sB) === 0.5} and apply action ${"a" === Var(mdp.actionName(a))}." +
      x" Calculate the new belief state."

    def solution() =
      SSnippet(newBelief(sA, sB, a).map { case (k, p) => ~ ("b'"(!k) === round(p)) }, ", ")
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