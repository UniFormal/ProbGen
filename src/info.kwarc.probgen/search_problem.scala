package info.kwarc.probgen

import SText._

/** a path in a transition system with states from S and actions from A */
case class Path[S,A](start: S, length: Int, _steps: List[(A,S)]) extends STeXAble {
  override def toString = start.toString + steps.map{case (a,s) => s" ~$a~> $s"}.mkString("")
  def rename[T,B](rs: S => T, ra: A => B) =
    Path(rs(start), length, _steps.map({case (s,a) => (ra(s),rs(a))}))
  def toSTeX = {
    val startS = SText(start.toString)
    val stepsS = steps.map {case (a,s) => x"\stackrel{\to}{$a}$s"}
    SMath(SSnippet(startS::stepsS))
  }
  def add(a: A, s: S) = Path(start, length+1, (a,s)::_steps)
  def last = if (_steps.isEmpty) start else _steps.head._2
  def steps = _steps.reverse
  def actions = steps.map(_._1)
}
object Path {
  def empty[S,A](i: S): Path[S,A] = Path(i, 0, Nil)
}

/** a search problem,
  * as defined in the lecture except for
  * - using type parameters for the origin of the states and actions
  * - choosing implementations for the sets: lists or predicates
  */
trait SearchProblem[S,A] {
  /** the set of states (must be a list so that they can be enumerated) */
  val states: List[S]
  /** the set of actions */
  val actions: List[A]
  /** the (non-deterministic) transition function */
  def trans(s:S, a:A): List[S]
  /** the initial states */
  val initial: List[S]
  /** the set of goal states (must be a predicate so that searching for them makes sense) */
  def goal(s: S): Boolean

  /** applies a list of actions in a state, returns the possible resulting states */
  def apply(from: List[S], path: List[A]): List[S] = {
    path match {
      case Nil => from
      case a::as =>
        val next = from.flatMap(c => trans(c,a)).distinct
        apply(next, as)
    }
  }

  /** checks if a list of actions is a solution */
  def check(path: List[A]) = apply(initial, path) exists goal

  /** finds solutions */
  def findSolutions(from: List[Path[S,A]], depth: Int, found: List[List[Path[S,A]]]): List[Path[S,A]] = {
    if (depth == 0) {
      return found.flatten
    }
    val (sols,other) = from.partition(p => goal(p.last))
    val next = other.flatMap {case p =>
      actions.flatMap(a => trans(p.last,a).map(t => p.add(a,t)))
    }
    findSolutions(next, depth-1, sols::found)
  }

  /** finds solutions up to a maximal depth */
  def solve(depth: Int) = {
    val start: List[Path[S,A]] = initial map Path.empty
    findSolutions(start, depth, Nil).sortBy(_.length)
  }

  val searchDepth = 8
  /** the solutions up to searchDepth */
  lazy val solutions = solve(searchDepth)
  /** the state-action pairs that are not applicable */
  lazy val inapplicableActions: List[(S,A)] = states.flatMap {s => actions.flatMap {a =>
    if (trans(s,a).isEmpty) List((s,a)) else Nil
  }}
}
object SearchProblem {
  /** the special case where states and actions are integers */
  type IntPath = Path[Int,Int]
  type IntSearchProblem = SearchProblem[Int,Int]
}

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

  /** computes the successor state by evaluating successor using the values s and a */
  def trans(s: Int,a: Int) = {
    val t = Evaluator(successor)(using Context("s" -> s)("a" -> a))
    List(t)
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
      x"as given by the table below",
      x"the states marked by §\to§ below",
      x"§s§ is marked by §!§ below"
    )}
    val part1 = SText(
      x"Consider the following search problem:\n",
      SItemize(
        SItem(x"set §S§ of states: ${FinSet(states.map(stateName)*)}"),
        SItem(x"set §A§ of actions: ${FinSet(actions.map(actionName)*)}"),
        SItem(x"transition relation §T§: $tr"),
        SItem(x"initial states §I§: $ins"),
        SItem(x"goal states §G§: ${InSet(Var("s"),Var("G"))} iff $go")
      )
    )
    val part2 = if (presentArithmetically) null else {
      val rowHeads = states.map {s =>
        val sN = stateName(s)
        if (initial.contains(s)) x"§\to§ $sN"
        else if (goal(s)) x"§!§ $sN"
        else x"$sN"
      }
      val cells = states.zipWithIndex.flatMap {case (s,i) =>
        actions.zipWithIndex.map {case (a,j) =>
          val t = trans(s,a).headOption.map(s => stateName(s).toSTeX).getOrElse(SText(" "))
          (i,j,t)
        }
      }
      SCenter(List(STabular(actions.map(a => actionName(a).toSTeX), rowHeads, cells)))
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
    override def applicable() = inapplicableActions.nonEmpty
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
        actionSeq = Generator.chooseSome(actions,2,3)
        result = apply(initial,actions)
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
      x"The solutions include ${namedSolutions.take(5)}."
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
      x"The solution(s) is/are $namedSolutions."
    }
  }

  GroupConstraint(1,1,giveSolution,giveAllSolutions)
}

/** randomly generates a search problem according to some criteria */
object SearchProblemGenerator {
  def make(): ExpressionBasedDeterminisiticSearchProblem = {
    // lower/upper bound for number of states
    val numStates = Generator.chooseInt(7,11)
    val stateList = Range(0,numStates).toList
    val states = FinSet(stateList.map(Lit)*)
    println("choosing states: " + states)
    // lower/upper bound for number of actions; actions are numbers similar in size to the states
    val actionList = Generator.chooseSome(stateList, 2, 4).sorted
    val actions = FinSet(actionList.map(Lit)*)
    println("choosing actions: " + actions)
    // the initial state is some state
    val initial = if (Generator.chooseBoolean(0.5)) 0 else numStates-1
    println("choosing initial state: " + initial)
    // repeat picking goal conditions until a good one is found
    var goal: Form = null
    var good = false
    println("choosing goal condition")
    while (!good) {
      // goal conditions are of the form p(s,i) where p is some predicate and i is a number
      val goalPred = Generator.choose(List(Less,LessEq,Divides,Equals))
      val goalArg = Generator.choose(Range(0,numStates).toList)
      goal = goalPred(Var("s"), Lit(goalArg))
      // compute the set of goal states and check if we like it
      val goalStates = Range(0,numStates).filter(s => Evaluator(goal)(using Context("s" -> s)))
      val numGoalStates = goalStates.length
      println("  " + goal + " --- " + "satisfied by " + goalStates.mkString(","))
      if (numGoalStates == 0) {
        println("    no goal states --- dismiss")
      } else if (numGoalStates.toDouble/numStates > 0.2) {
        println("    too many goal states --- dismiss")
      } else if (goalStates.contains(initial)) {
        println("    initial is goal --- dismiss")
      } else {
        good = true
        if (numGoalStates == 1) {
          // if there is only one goal state, use the equality as the predicate
          goal = Equals(Var("s"), Lit(goalStates.head))
        }
      }
    }
    // loop until a good transition function is found
    println("choosing transition operation")
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
      println("  " + term2)
      val trans = Mod(term2,Lit(numStates))
      searchProb = ExpressionBasedDeterminisiticSearchProblem(numStates,actionList,trans,initial,goal)
      // solve the resulting search problem and check if we like the solutions
      val solutions = searchProb.solutions
      if (solutions.isEmpty) {
        println("  no solutions --- dismiss")
      } else {
        val sol = solutions.head
        println("  shortest solutions: " + sol)
        if (sol.length < 4) {
          println("    length under 4 --- dismiss")
        } else if (sol.actions.distinct.length < 2) {
          println("    uses only 1 action --- dismiss")
        } else {
          println("  solutions: " + solutions.take(5).mkString("\n  "))
          good = true
        }
      }
    }
    if (!good) {
      // fail-safe: start from scratch if we're stuck
      println("no problem found, trying again")
      return make()
    }
    searchProb.presentArithmetically = Generator.chooseBoolean(0.5)
    println("chosen problem: " + searchProb)
    searchProb
  }
}
