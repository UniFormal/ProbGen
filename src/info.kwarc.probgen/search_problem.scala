package info.kwarc.probgen

import SText._

case class Path[S,A](start: S, length: Int, _steps: List[(A,S)]) extends STeXAble {
  override def toString = start.toString + steps.map{case (a,s) => s" ~$a~> $s"}.mkString("")
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

object SearchProblem {
  type IntPath = Path[Int,Int]
  type IntSearchProblem = SearchProblem[Int,Int]
}
trait SearchProblem[S,A] {
  val states: List[S]
  val actions: List[A]
  def trans(s:S, a:A): List[S]
  val initial: List[S]
  def goal(s: S): Boolean

  def apply(from: List[S], path: List[A]): List[S] = {
    path match {
      case Nil => from
      case a::as =>
        val next = from.flatMap(c => trans(c,a)).distinct
        apply(next, as)
    }
  }

  def check(path: List[A]) = apply(initial, path) exists goal

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

  def solve(depth: Int) = {
    val start: List[Path[S,A]] = initial map Path.empty
    findSolutions(start, depth, Nil).sortBy(_.length)
  }

  val searchDepth = 8
  lazy val solutions = solve(searchDepth)
  lazy val inapplicableActions: List[(S,A)] = states.flatMap {s => actions.flatMap {a =>
    if (trans(s,a).isEmpty) List((s,a)) else Nil
  }}
}

case class ExpressionBasedDeterminisiticSearchProblem(numStates: Int, actions: List[Int], successor: Term, init: Int, goalForm: Form)
  extends SearchProblem[Int,Int] with Problem[ExpressionBasedDeterminisiticSearchProblem] {
  val states = Range(0,numStates).toList
  val initial = List(init)

  def trans(s: Int,a: Int) = {
    val t = Evaluator(successor)(Context("s" -> s)("a" -> a))
    List(t)
  }

  def goal(s: Int) = {
    Evaluator(goalForm)(Context("s" -> s))
  }

  def intro() = {
    SText(
      x"Consider the following search problem:\n",
      SItemize(
        SItem(x"set §S§ of states: ${FinSet(states)}"),
        SItem(x"set §A§ of actions: ${FinSet(actions)}"),
        SItem(x"transition relation §T§: ${GivenBy("T",List("s","a"), FinSet(successor))} (where all operations are taken modulo $numStates)"),
        SItem(x"initial states §I§: ${FinSet(initial)}"),
        SItem(x"goal states §G§: ${InSet(Var("s"),Var("G"))} iff $goalForm")
      )
    )
  }

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

  val _ = GroupConstraint(1,1,whyDet,whyPO)

  object actionNotApplicable extends Subproblem("apply",1,2) {
    override def applicable() = inapplicableActions.nonEmpty
    def question() = {
      x"Give an example of a state §s§ and an action §a§ such that §a§ is not applicable in §s§."
    }
    def solution() = {
      x"The correct answers §(s,a)§ are $inapplicableActions"
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
    var result: List[Int] = null

    override def init() = {
      do {
        actionSeq = Generator.chooseSome(actions,2,3)
        result = apply(initial,actions)
      } while (result.isEmpty)
    }

    def question() = {
      x"Give the state(s) that can be reached by applying the action sequence ${FinSeq(actionSeq)} in an initial state."
    }

    def solution() = {
      x"The possible states are ${FinSeq(result)}."
    }
  }

  val _ = GroupConstraint(1,2, allActionsApplicable, actionNotApplicable, applyAction)

  object giveSolution extends Subproblem("apply",2,5) {
    override def applicable() = solutions.head.length <= 6
    def question() = {
      x"Give a solution."
    }
    def solution() = {
      x"The solutions include ${solutions.take(5)}."
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
      x"The solution(s) is/are $solutions."
    }
  }

  GroupConstraint(1,1,giveSolution,giveAllSolutions)
}

object SearchProblemGenerator {
  def make(): ExpressionBasedDeterminisiticSearchProblem = {
    val numStates = Generator.chooseInt(7,11)
    val stateList = Range(0,numStates).toList
    val states = FinSet(stateList.map(Lit):_*)
    println("choosing states: " + states)
    val actionList = Generator.chooseSome(stateList, 2, 4).sorted
    val actions = FinSet(actionList.map(Lit):_*)
    println("choosing actions: " + actions)
    val initial = if (Generator.chooseBoolean(0.5)) 0 else numStates-1
    println("choosing initial state: " + initial)
    var goal: Form = null
    var good = false
    println("choosing goal condition")
    do {
      val goalPred = Generator.choose(List(Less,LessEq,Divides,Equals))
      val goalArg = Generator.choose(Range(0,numStates).toList)
      goal = goalPred(Var("s"), Lit(goalArg))
      val goalStates = Range(0,numStates).filter(s => Evaluator(goal)(Context("s" -> s)))
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
          // normalize trivial goal predicate
          goal = Equals(Var("s"), Lit(goalStates.head))
        }
      }
    } while (!good)
    println("choosing transition operation")
    var searchProb: ExpressionBasedDeterminisiticSearchProblem = null
    good = false
    var tries = 0
    do {
      tries += 1
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
    } while (!good && tries < 100)
    if (!good) {
      println("no problem found, trying again")
      return make()
    }
    println("chosen problem: " + searchProb)
    searchProb
  }
}

