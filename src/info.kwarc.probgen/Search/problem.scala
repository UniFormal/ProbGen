package info.kwarc.probgen

import SText._
import Expr._

case class ExpressionBasedDeterministicSearchProblem(
  numStates: Int, actions: List[Int], successor: Expr, init: Int, goalForm: Expr, maxSearchDepth: Int, presentArithmetically: Boolean
) extends SearchProblem[Int,Int] with Problem[ExpressionBasedDeterministicSearchProblem] {

  val states  = Range(0, numStates).toList
  val initial = List(init)

  // ScalaJS requires explicit override of lazy val from trait
  override lazy val solutions = solve(maxSearchDepth)

  def trans(s: Int, a: Int) = {
    val t = Evaluator(successor)(using Context("s" -> s)("a" -> a)).asInt
    if (states.contains(t)) List(t) else Nil
  }

  def goal(s: Int) = Evaluator(goalForm)(using Context("s" -> s)).asBool

  def stateName(s: Int): Expr  = if (presentArithmetically) DInt(s) else NameLit(s)
  def actionName(a: Int): Expr = if (presentArithmetically) DInt(a)
  else NameLit(26 - actions.length + actions.indexOf(a))
  def namedSolutions = solutions.map(_.rename(stateName, actionName))

  // ── Reading names back ───────────────────────────────────────────────────
  // a typed name is looked up among the shown names, so numbers and letters both work

  private def stateOf(t: Expr): Either[String, Int] =
    states.find(s => ExprCompare.sameValue(stateName(s), t))
      .toRight(s"<code>${Html.esc(ExprCompare.show(t))}</code> is not a state of this problem.")

  private def actionOf(t: Expr): Either[String, Int] =
    actions.find(a => ExprCompare.sameValue(actionName(a), t))
      .toRight(s"<code>${Html.esc(ExprCompare.show(t))}</code> is not an action of this problem. " +
        s"The actions are ${Html.codeList(actions.map(a => ExprCompare.show(actionName(a))))}.")

  /** a sequence of actions, e.g. "x, y, x" (brackets optional) */
  private def readActions(input: String): Either[String, List[Int]] =
    ExprParser.parseExprList(input).flatMap(ts => Grader.sequence(ts)(actionOf))

  /** how an action sequence is shown in hints */
  private def showActions(as: List[Int]): String = as.map(a => ExprCompare.show(actionName(a))).mkString(", ")

  private def formatPath(p: Path[Expr, Expr]) = {
    val startState = x"${p.start}"
    val formattedSteps = p.steps.map { case (action, nextState) => 
      x" -$action-> $nextState" 
    }
    SText((startState :: formattedSteps)*)
  }

  def intro() = {
    val (tr, ins, go) = if (presentArithmetically) {
      (~("T"("s","a") === FinSet(successor)) + x"(where all operations are taken modulo $numStates)",
        ~FinSet(initial),
        ~goalForm)
    } else {(
      x"as given by the table below (where empty cells indicate inapplicable actions)",
      x"the states marked by ${Var("\\rightarrow")} below",
      x"${Var("s")} is marked by ${Var("!")} below"
    )}
    val part1 = SText(
      x"Consider the following search problem:\n",
      SItemize(
        x"set ${Var("S")} of states: ${FinSet(states.map(stateName)*)}",
        x"set ${Var("A")} of actions: ${FinSet(actions.map(actionName)*)}",
        x"transition relation ${Var("T")}: $tr",
        x"initial states ${Var("I")}: $ins",
        x"goal states ${Var("G")}: ${InSet(Var("s"),Var("G"))} iff $go"
      )
    )
    val part2 = if (presentArithmetically) null else {
      val rowHeads = states.map { s =>
        val sN = stateName(s)
        if (initial.contains(s)) x"${Var("\\rightarrow")} $sN"
        else if (goal(s)) x"$sN ${Var("!")} "
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
    override def answerHint = "in words"
    override def rubric = Some(AnswerRubric.of(
      AnswerClass(1, "Says that there is exactly one initial state (the set of initial states has size 1).")))
    def question() = x"This search problem is fully observable. How can we tell?"
    def solution() = x"The set of initial states has size ${1}."
  }

  object whyDet extends Subproblem("apply", 1, 2) {
    override def answerHint = "in words"
    override def rubric = Some(AnswerRubric.of(
      AnswerClass(1, "Says that every transition T(s, a) gives at most one next state.")))
    def question() = x"This search problem is deterministic. How can we tell?"
    def solution() = x"The transition model always returns a set containing at most one element."
  }

  GroupConstraint(1, 1, whyDet, whyPO)

  object actionNotApplicable extends Subproblem("apply", 1, 2) {
    override def applicable() = presentArithmetically && inapplicableActions.nonEmpty
    override def answerHint = "a pair (state, action), e.g. (3, 5)"
    // many pairs are right, so the transition model decides
    override def grader = Some(VerifiedGrader[(Int, Int)](
      input => ExprParser.parseExprList(input).flatMap {
        case List(Tuple(Seq(s, a))) => for { st <- stateOf(s); ac <- actionOf(a) } yield (st, ac)
        case List(s, a)             => for { st <- stateOf(s); ac <- actionOf(a) } yield (st, ac)
        case _                      => Left("Give one pair (state, action), e.g. (3, 5).")
      },
      { case (s, a) => trans(s, a).isEmpty },
      "That action is applicable in that state: the transition model gives a next state."))
    def question() = x"Give an example of a state ${Var("s")} and an action ${Var("a")} such that ${Var("a")} is not applicable in ${Var("s")}."
    def solution() = x"The correct answers ${Tuple(Var("s"), Var("a"))} are ${inapplicableActions.map(sa => (stateName(sa._1), actionName(sa._2)))}"
  }

  object allActionsApplicable extends Subproblem("apply", 1, 2) {
    override def applicable() = inapplicableActions.isEmpty
    override def answerHint = "in words"
    override def rubric = Some(AnswerRubric.of(
      AnswerClass(1, "Says that T(s, a) is never empty: every action leads to a next state in every state.")))
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
    override def answerHint = s"a set of states, e.g. {${ExprCompare.show(stateName(states.last))}}"
    override def grader = Some(ExprSetGrader(result.map(stateName), "state"))
    def question() = x"Give the state(s) that can be reached by applying the action sequence ${FinSeq(actionSeq.map(actionName)*)} in an initial state."
    def solution() = x"${FinSeq(result.map(stateName)*)}"
  }

  GroupConstraint(1, 2, allActionsApplicable, actionNotApplicable, applyAction)

  object giveSolution extends Subproblem("apply", 2, 5) {
    override def applicable() = solutions.head.length <= 6
    override def answerHint = s"a sequence of actions, e.g. ${showActions(actions.take(2))}"

    def question() = x"Give a solution"

    def solution() = x"The solutions include ${SItemize(namedSolutions.take(5).map(p => ~p.toExpr)*)}."

    override def grader = Some(input => Grader.onParsed(readActions(input)) { as =>
      val reached = apply(initial, as)
      if (reached.isEmpty) Incorrect("This action sequence cannot be carried out: on the way, an action is not applicable.")
      else if (reached.exists(goal)) Correct()
      else Incorrect("That action sequence does not reach a goal state.")
    })
  }

  object giveAllSolutions extends Subproblem("apply", 3, 5) {
    override def applicable() = 2 <= solutions.length && solutions.length <= 3
    override def answerHint = s"one action sequence per solution, separated by ';', e.g. ${showActions(actions.take(2))}; ${showActions(actions.take(1))}"
    override def grader = Some(input => {
      val parts = input.split("[;\n]").toList.map(_.trim).filter(_.nonEmpty)
      Grader.onParsed(Grader.sequence(parts)(readActions)) { answered =>
        Grader.toResult(Grader.compareSets(answered, solutions.map(_.actions).toSet, showActions, false, "solution"))
      }
    })
    def question() = {
      val n      = solutions.length
      x"Give all $n solutions whose length is at most $maxSearchDepth."
    }
    def solution() = x"The solution(s) is/are ${SItemize(namedSolutions.map(formatPath)*)}."
  }

  GroupConstraint(1, 1, giveSolution, giveAllSolutions)
}