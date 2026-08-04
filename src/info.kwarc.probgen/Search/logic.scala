package info.kwarc.probgen

import SText._

/** a path in a transition system with states from S and actions from A */
case class Path[S,A](start: S, length: Int, _steps: List[(A,S)]) extends ExprLike {
  override def toString = start.toString + steps.map{case (a,s) => s" ~$a~> $s"}.mkString("")
  def rename[T,B](rs: S => T, ra: A => B) =
    Path(rs(start), length, _steps.map({case (s,a) => (ra(s),rs(a))}))
  def toExpr = {
    val stepsM = steps.flatMap {case (a,s) => List(Expr(a), Expr(s))}
    TransitionChain(Expr(start)::stepsM *)
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
  /** the maximum depth we are willing to search for solutions */
  val maxSearchDepth: Int

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

  /** the solutions up to maxSearchDepth */
  lazy val solutions = solve(maxSearchDepth)
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