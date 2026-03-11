package info.kwarc.probgen

import SText._

/** a concrete representation of a deterministic fully observable search problem that can be randomly generated
  * @param numStates states are {0,...,numStates}
  * @param actions set of actions
  * @param successor a term in variables "s" and "a" that gives the successor state
  * @param init the initial state
  * @param goalForm a formula in variable "s" that expresses if s is a goal
  */
case class ExpressionBasedDeterminisiticSearchProblem(numStates: Int, actions: List[Int], successor: Term, init: Int, goalForm: Form)
  extends SearchProblem[Int,Int] with Problem[ExpressionBasedDeterminisiticSearchProblem] {
  val states = Range(0,numStates).toList
  val initial = List(init)

  var presentArithmetically = true

  /** computes the successor state by evaluating successor using the values s and a
    * Actions are only applicable if the transition results in a legal state.
    */
  def trans(s: Int,a: Int) = {
    val t = Evaluator(successor)(using Context("s" -> s)("a" -> a))
    if (states contains t) List(t) else Nil
  }

  /** checks if a state is a goal state by evaluating goalFrom using the value s */
  def goal(s: Int) = {
    Evaluator(goalForm)(using Context("s" -> s))
  }

  private def stateName(s: Int): Expr = if (presentArithmetically) Lit(s) else NameLit(s)
  private def actionName(a: Int): Expr = if (presentArithmetically) Lit(a) else NameLit(26-actions.length+actions.indexOf(a))
  def namedSolutions = solutions.map(_.rename(stateName,actionName))

  /** renders the intro text of the problem */
  def intro() = {
    // use x" ..." to generate tex syntax (Scala string interpolation)
    // use $variable or ${expression} to insert other objects
    // - expressions are automatically converted into stex syntax
    // - integers, strings etc. are inserted as is
    // use § instead of $ for tex math mode
    val (tr,ins,go) = if (presentArithmetically) {
      (x"${GivenBy("T",List("s","a"), FinSet(successor))} (where all operations are taken modulo $numStates)",
       FinSet(initial),
       goalForm
      )
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
      val rowHeads = states.map {s =>
        val sN = stateName(s)
        if (initial.contains(s)) x"§\to§ $sN"
        else if (goal(s)) x"$sN §!§ "
        else x"$sN"
      }
      val cells = states.zipWithIndex.flatMap {case (s,i) =>
        actions.zipWithIndex.map {case (a,j) =>
          val t = trans(s,a).headOption.map(s => stateName(s).toSTeXTop).getOrElse(SText(" "))
          (i,j,t)
        }
      }
      val colHeads = actions.map(a => actionName(a).toSTeXTop)
      SCenter(List(STabular(colHeads, rowHeads, cells)))
    }
    part1 + part2
  }

  // ***** subproblems ******

  /** a subproblem with a question and solution */
  object whyPO extends Subproblem("apply",1,2) {
    def question() = {
      x"This search problem is fully observable. How can we tell?"
    }
    def solution() = {
      x"The set of initial states has size §1§."
    }
  }

  object whyDet extends Subproblem("apply",1,2) {
    def question() = {
      x"This search problem is determinisitic. How can we tell?"
    }
    def solution() = {
      x"The transition model always returns a set containing at most one element."
    }
  }

  /** a constraint to indicate that exactly one out of the above subproblems should be chosen */
  GroupConstraint(1,1,whyDet,whyPO)

  object actionNotApplicable extends Subproblem("apply",1,2) {
    override def applicable() = presentArithmetically && inapplicableActions.nonEmpty
    def question() = {
      x"Give an example of a state §s§ and an action §a§ such that §a§ is not applicable in §s§."
    }
    def solution() = {
      x"The correct answers §(s,a)§ are ${inapplicableActions.map(sa => (stateName(sa._1),actionName(sa._2)))}"
    }
  }

  object allActionsApplicable extends Subproblem("apply",1,2) {
    override def applicable() = inapplicableActions.isEmpty
    def question() = {
      x"In this search problem, every action is applicable in every state. How can we tell?"
    }
    def solution() = {
      x"The transition model never returns the empty set."
    }
  }

  object applyAction extends Subproblem("apply",3,5) {
    var actionSeq: List[Int] = null
    var result: List[Int] = Nil

    override def init() = {
      while (result.isEmpty) {
        actionSeq = Generator.chooseSome(actions,2,3,true)
        result = apply(initial,actionSeq)
      }
    }

    def question() = {
      x"Give the state(s) that can be reached by applying the action sequence ${FinSeq(actionSeq.map(actionName)*)} in an initial state."
    }

    def solution() = {
      x"The possible states are ${FinSeq(result.map(stateName)*)}."
    }
  }

  GroupConstraint(1,2, allActionsApplicable, actionNotApplicable, applyAction)

  object giveSolution extends Subproblem("apply",2,5) {
    override def applicable() = solutions.head.length <= 6
    def question() = {
      x"Give a solution."
    }
    def solution() = {
      x"The solutions include ${SItemize(namedSolutions.take(5).map(SText.make)*)}."
    }
  }

  object giveAllSolutions extends Subproblem("apply",3,5) {
    override def applicable() = solutions.length <= 3
    def question() = {
      val n = solutions.length
      val plural = if (n > 1) "s" else ""
      x"Give all $n solution$plural whose length is at most $searchDepth."
    }
    def solution() = {
      x"The solution(s) is/are ${SItemize(namedSolutions.map(SText.make)*)}."
    }
  }

  GroupConstraint(1,1,giveSolution,giveAllSolutions)
}
