package info.kwarc.probgen

import SText._
import Expr._

case class ExpressionBasedDeterministicSearchProblem(
                                                       numStates: Int, actions: List[Int], successor: Term, init: Int, goalForm: Form, maxSearchDepth: Int, presentArithmetically: Boolean
                                                     ) extends SearchProblem[Int,Int] with Problem[ExpressionBasedDeterministicSearchProblem] {

  val states  = Range(0, numStates).toList
  val initial = List(init)

  // ScalaJS requires explicit override of lazy val from trait
  override lazy val solutions = solve(maxSearchDepth)

  def trans(s: Int, a: Int) = {
    val t = Evaluator(successor)(using Context("s" -> s)("a" -> a)).asInt
    if (states.contains(t)) List(t) else Nil
  }

  def goal(s: Int) = Evaluator(goalForm)(using Context("s" -> s))

  private def stateName(s: Int): Term  = if (presentArithmetically) DInt(s) else NameLit(s)
  private def actionName(a: Int): Term = if (presentArithmetically) DInt(a)
  else NameLit(26 - actions.length + actions.indexOf(a))
  def namedSolutions = solutions.map(_.rename(stateName, actionName))

  private def formatPath(p: Path[Term, Term]) = {
    val startState = x"${p.start}"
    val formattedSteps = p.steps.map { case (action, nextState) => 
      x" -$action-> $nextState" 
    }
    SText((startState :: formattedSteps)*)
  }

  // Parse a user-typed action token into an Int action value.
  // In arithmetic mode actions are integers; in named mode they are letters (a, b, c...).
  private def parseAction(token: String): Option[Int] = {
    val t = token.trim
    if (t.matches("-?\\d+")) {
      // Integer token — check it's in our action list
      val v = t.toInt
      if (actions.contains(v)) Some(v) else None
    } else if (t.length == 1 && t.charAt(0) >= 'a' && t.charAt(0) <= 'z') {
      // Letter token — reverse the mapping from actionName
      // actionName uses: charIdx = 26 - actions.length + listIdx
      // Therefore:       listIdx = charIdx - 26 + actions.length
      val charIdx = t.charAt(0) - 'a'
      val listIdx = charIdx - 26 + actions.length
      
      if (listIdx >= 0 && listIdx < actions.length) Some(actions(listIdx)) else None
    } else None
  }
  
  private def parseCommaSeparatedList(input: String): List[Int] = {
    val tokens = input.split("[,\\s]+").map(_.trim).filter(_.nonEmpty).toList
    if (tokens.isEmpty) throw new RuntimeException("Input is empty.")
    tokens.map { t =>
      parseAction(t).getOrElse(
        throw new RuntimeException(s"'$t' is not a valid action for this problem.")
      )
    }
  }

  def intro() = {
    val (tr, ins, go) = if (presentArithmetically) {
      (~("T"("s","a") === FinSet(successor)) + x"(where all operations are taken modulo $numStates)",
        ~FinSet(initial),
        ~goalForm)
    } else {(
      x"as given by the table below (where empty cells indicate inapplicable actions)",
      x"the states marked by §\to§ below",
      x"§s§ is marked by §!§ below"
    )}
    val part1 = SText(
      x"Consider the following search problem:\n",
      SItemize(
        x"set §S§ of states: ${FinSet(states.map(stateName)*)}",
        x"set §A§ of actions: ${FinSet(actions.map(actionName)*)}",
        x"transition relation §T§: $tr",
        x"initial states §I§: $ins",
        x"goal states §G§: ${InSet(Var("s"),Var("G"))} iff $go"
      )
    )
    val part2 = if (presentArithmetically) null else {
      val rowHeads = states.map { s =>
        val sN = stateName(s)
        if (initial.contains(s)) x"§\to§ $sN"
        else if (goal(s)) x"$sN §!§ "
        else x"$sN"
      }
      val cells = states.zipWithIndex.flatMap { case (s,i) =>
        actions.zipWithIndex.map { case (a,j) =>
          val t = trans(s,a).headOption.map(s => ~stateName(s)).getOrElse(SText(" "))
          (i, j, t)
        }
      }
      val colHeads = actions.map(a => ~actionName(a))
      SCenter(List(STabular(SText(""), colHeads, rowHeads, cells)))
    }
    part1 + part2
  }

  // ── Subproblems ────────────────────────────────────────────────────────

  object whyPO extends Subproblem("apply", 1, 2) {
    def question() = x"This search problem is fully observable. How can we tell?"
    def solution() = x"The set of initial states has size §1§."
  }

  object whyDet extends Subproblem("apply", 1, 2) {
    def question() = x"This search problem is deterministic. How can we tell?"
    def solution() = x"The transition model always returns a set containing at most one element."
  }

  GroupConstraint(1, 1, whyDet, whyPO)

  object actionNotApplicable extends Subproblem("apply", 1, 2) {
    override def applicable() = presentArithmetically && inapplicableActions.nonEmpty
    def question() = x"Give an example of a state §s§ and an action §a§ such that §a§ is not applicable in §s§."
    def solution() = x"The correct answers §(s,a)§ are ${inapplicableActions.map(sa => (stateName(sa._1), actionName(sa._2)))}"
  }

  object allActionsApplicable extends Subproblem("apply", 1, 2) {
    override def applicable() = inapplicableActions.isEmpty
    def question() = x"In this search problem, every action is applicable in every state. How can we tell?"
    def solution() = x"The transition model never returns the empty set."
  }

  object applyAction extends Subproblem("apply", 3, 5) {
    var actionSeq: List[Int] = null
    var result: List[Int]    = Nil
    override def init() = {
      while (result.isEmpty) {
        actionSeq = Generator.chooseSome(actions, 2, 3, true)
        result    = apply(initial, actionSeq)
      }
    }
    def question() = x"Give the state(s) that can be reached by applying the action sequence ${FinSeq(actionSeq.map(actionName)*)} in an initial state."
    def solution() = x"${FinSeq(result.map(stateName)*)}"
  }

  GroupConstraint(1, 2, allActionsApplicable, actionNotApplicable, applyAction)

  // giveSolution: uses the professor's checkSolution logic
  object giveSolution extends Subproblem("apply", 2, 5) {
    override def applicable() = solutions.head.length <= 6

    def question() = x"Give a solution"
    
    // this function is unused as we overwrite checkSolution
    def solution() = x"The solutions include ${SItemize(namedSolutions.take(5).map(p => ~p.toExpr)*)}."

    // Override with domain-specific checking:
    // parse the input as a comma-separated action sequence and verify it reaches a goal.
    override def checkSolution(input: String): CheckResult = {
      val as = try { parseCommaSeparatedList(input) }
      catch { case e: RuntimeException =>
        return Incorrect("A solution is a comma-separated list of actions. " + e.getMessage)
      }
      if (as.isEmpty)
        return Incorrect("A solution is a comma-separated list of actions.")
      val resultingStates = apply(initial, as)
      if (resultingStates.isEmpty)
        return Incorrect("This action sequence is not applicable in the initial state.")
      if (resultingStates.exists(s => goal(s))) Correct()
      else Incorrect("That action sequence does not reach a goal state.")
    }
  }

  object giveAllSolutions extends Subproblem("apply", 3, 5) {
    override def applicable() = 2 <= solutions.length && solutions.length <= 3
    def question() = {
      val n      = solutions.length
      x"Give all $n solutions whose length is at most $maxSearchDepth."
    }
    def solution() = x"The solution(s) is/are ${SItemize(namedSolutions.map(formatPath)*)}."
  }

  GroupConstraint(1, 1, giveSolution, giveAllSolutions)
}