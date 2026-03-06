package info.kwarc.probgen

trait ProblemGenerator[PD <: Problem[PD]] {
  def log(s: String) = println("% " + s)
  def make(): PD
}

/**
  * parent class of exam-style problems
  *
  * Each individual problem should subclass this trait and then declare in its body some objects that subclass Subproblem
  * as well as some fields whose value is GroupConstraint as needed.
  *
  * An actual exam problem will use some of these subproblems in a way that meets the dependencies and group constraints.
  *
  * See [[ExpressionBasedDeterminisiticSearchProblem]] for an example.
  */
trait Problem[PD <: Problem[PD]] {
  /** intro text */
  def intro(): SText

  private var subproblems: List[Subproblem] = Nil
  private var groupConstraints: List[GroupConstraint] = Nil

  /** a subproblem */
  abstract class Subproblem(val id: String, pts: Int, testspace: Int) {
    subproblems = subproblems ::: List(this)
    /** other subproblems, given by id, that must be selected if this subproblem is used */
    def dependencies: List[String] = Nil
    /** true if this subproblem is applicable;
      * all values needed to determine this information should be fields of the implementing class */
    def applicable(): Boolean = true

    /** initialization, e.g., to pick random values for this subproblem */
    def init(): Unit = {}

    /** question text */
    def question(): SText
    /** solution text */
    def solution(): SText

    def toSTeX() = {
      SSubproblem(pts, question(), SSolution(testspace, List(solution())))
    }
  }

  /** constrains how many subproblems are chosen from a subset of subproblems (which must be declared in this class) */
  case class GroupConstraint(atLeast: Int, atMost: Int, choices: Subproblem*) {
    groupConstraints ::= this
    val length = choices.length
  }

  def chooseSubproblems(): List[Subproblem] = {
    var subs = subproblems.filter(_.applicable())
    groupConstraints.foreach {gc =>
      val currentlyChosen = subs.filter(p => gc.choices.contains(p))
      if (currentlyChosen.length > gc.atMost) {
        val numRemove = currentlyChosen.length - gc.atMost
        val remove = Generator.chooseSome(currentlyChosen, numRemove,numRemove,false)
        subs = subs.diff(remove)
      }
    }
    subs.map(_.init())
    subs
  }

  def toSTeX(subs: List[Subproblem]) = {
    SProblem(intro(), subs.map(_.toSTeX()))
  }
  def toSTeXAll() = {
    val subs = subproblems
    subs.map(_.init())
    toSTeX(subs)
  }
}
