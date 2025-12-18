package info.kwarc.probgen

import SText._

case class Path[S,A](start: S, length: Int, _steps: List[(A,S)]) {
  override def toString = start.toString + steps.map{case (a,s) => s" ~$a~> $s"}.mkString("")
  def add(a: A, s: S) = Path(start, length+1, (a,s)::_steps)
  def last = if (_steps.isEmpty) start else _steps.head._2
  def steps = _steps.reverse
}
object Path {
  def empty[S,A](i: S): Path[S,A] = Path(i, 0, Nil)
}

object SearchProblem {
  type IntPath = Path[Int,Int]
  type IntSearchProblem = SearchProblem[Int,Int]
}
import SearchProblem._

trait SearchProblem[S,A] {
  val states: List[Int]
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
    findSolutions(start, depth, Nil)
  }
}

case class ExpressionBasedDeterminisiticSearchProblem(numStates: Int, numActions: Int, successor: Term, init: Int, goalForm: Form)
  extends SearchProblem[Int,Int] {
  val states = Range(0,numStates).toList
  val actions = Range(0,numActions).toList
  val initial = List(init)
  def trans(s: Int, a: Int) = {
    val t = Evaluator(successor)(Context("s" -> s)("a" -> a))
    List(t % numStates)
  }
  def goal(s: Int) = {
    Evaluator(goalForm)(Context("s" -> s))
  }
}

object SearchProblemGenerator {
  def make() = {
    val numStates = Generator.chooseInt(4,11)
    val states = FinSet(Range(0,numStates).map(Lit):_*)
    val numActions = Generator.chooseInt(2,4)
    val actions = FinSet(Range(0,numActions).map(Lit):_*)
    val initial = if (Generator.chooseBoolean(0.5)) 0 else numStates-1
    val goalShape = new State(2,3,List("s"))
    var goal: Form = null
    var good = false
    do {
      goal = Generator.genForm(goalShape)
      val goalStates = Range(0,numStates).filter(s => Evaluator(goal)(Context("s" -> s)))
      val numGoalStates = goalStates.length
      good = (numGoalStates > 0 && numGoalStates <= 3) && !goalStates.contains(initial)
    } while (!good)
    var searchProb: ExpressionBasedDeterminisiticSearchProblem = null
    var solutions: List[IntPath] = null
    good = false
    do {
      val transShape = new State(2,3,List("s","a"))
      val trans = Generator.genTerm(transShape)
      searchProb = new ExpressionBasedDeterminisiticSearchProblem(numStates,numActions,trans,initial,goal)
      solutions = searchProb.solve(8).sortBy(_.length)
      good = solutions.nonEmpty && solutions.head.length > 3
    } while (!good)
    println(searchProb)
    println(solutions.mkString("\n"))
    val question = SText(
      x"Consider the following search problem:",
      SItemize(
        SItem(x"set §S§ of states: $states"),
        SItem(x"set §A§ of actions: $actions"),
        SItem(x"transition relation §T§: §T(s,a)=${FinSet(searchProb.successor)}"),
        SItem(x"initial states §I§: ${FinSet(Lit(initial))}"),
        SItem(x"goal states §G§: §\\inset gG§ iff ${searchProb.goalForm}")
      )
    )
    SProblem(question, Nil)
  }
}

