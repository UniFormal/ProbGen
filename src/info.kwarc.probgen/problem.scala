package info.kwarc.probgen

// ---------------------------------------------------------------------------
// Result type for answer checking
// ---------------------------------------------------------------------------
abstract class CheckResult
case class Correct() extends CheckResult
case class NotCheckable(expectedSolution: String) extends CheckResult
case class Incorrect(hint: String) extends CheckResult

// ---------------------------------------------------------------------------
// Generator base
// ---------------------------------------------------------------------------
trait ProblemGenerator[PD <: Problem[PD]] {
  def log(s: String) = println("% " + s)
  def make(): PD
}

// ---------------------------------------------------------------------------
// Problem base
// ---------------------------------------------------------------------------
trait Problem[PD <: Problem[PD]] {
  def intro(): SText

  private var subproblems: List[Subproblem] = Nil
  private var groupConstraints: List[GroupConstraint] = Nil

  abstract class Subproblem(val id: String, pts: Int, testspace: Int) {
    subproblems = subproblems ::: List(this)

    def dependencies: List[String] = Nil
    def applicable(): Boolean = true
    def init(): Unit = {}
    def question(): SText
    def solution(): SText

    // Default check: compare plain-text normalisation of input vs solution.
    // Subclasses override this to provide domain-specific checking with hints.
    def checkSolution(input: String): CheckResult = {
      val expected = solution().toText.trim
      if (normalise(input) == normalise(expected)) Correct()
      else NotCheckable(expected)
    }

    // Normalise for lenient string comparison:
    // lowercase, strip spaces, sort additive terms
    protected def normalise(s: String): String = {
      val clean = s.toLowerCase.trim.split("\\s+").mkString("")
      clean.split("\\+").map(_.trim).filter(_.nonEmpty).sorted.mkString("+")
    }

    def toSTeX() = SSubproblem(
      pts,
      question(),
      SSolution(testspace, List(solution())),
      this.hashCode.abs.toString
    )
  }

  case class GroupConstraint(atLeast: Int, atMost: Int, choices: Subproblem*) {
    groupConstraints ::= this
    val length = choices.length
  }

  def chooseSubproblems(): List[Subproblem] = {
    var subs = subproblems.filter(_.applicable())
    groupConstraints.foreach { gc =>
      val currentlyChosen = subs.filter(p => gc.choices.contains(p))
      if (currentlyChosen.length > gc.atMost) {
        val numRemove = currentlyChosen.length - gc.atMost
        val remove =
          Generator.chooseSome(currentlyChosen, numRemove, numRemove, false)
        subs = subs.diff(remove)
      }
    }
    subs.map(_.init())
    subs
  }

  def toSTeX(subs: List[Subproblem]) = SProblem(intro(), subs.map(_.toSTeX()))
  def toSTeXAll() = { val subs = subproblems; subs.map(_.init()); toSTeX(subs) }
}
