package info.kwarc.probgen

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

  abstract class Subproblem(val id: String, val pts: Int, val testspace: Int) {
    subproblems = subproblems ::: List(this)

    def dependencies: List[String] = Nil
    def applicable(): Boolean = true
    def init(): Unit = {}
    def question(): SText
    def solution(): SText

    /**
      * Override to have this subproblem graded automatically. See answers.scala
      * for the available graders. This is a `def`, not a `val`, so that it is
      * evaluated after `init()` has bound the subproblem's random choices.
      */
    def grader: Option[Grader] = None

    /**
      * Override to attach a grading scheme to a question that cannot be graded
      * automatically. Used both for human marking of the printed sheet and for
      * self-assessment in the browser.
      */
    def rubric: Option[AnswerRubric] = None

    /**
      * Checking order: an explicit grader wins, then a rubric, and otherwise the
      * question is simply reported as not auto-graded. Subclasses may override
      * this method directly for fully bespoke checking.
      *
      * There is deliberately no string comparison here. Comparing what a student
      * typed against the *rendered* solution is not a semantic check: it fails on
      * any difference of spacing, ordering or notation, so it was reporting wrong
      * answers as often as right ones. A subproblem is graded when it says how,
      * via `grader` (see answers.scala) or `rubric`, and otherwise it is honest
      * about not being graded yet.
      */
    def checkSolution(input: String): CheckResult =
      if (input.trim.isEmpty) MalformedInput("Please enter an answer first.")
      else
        grader match {
          case Some(g) => g.grade(input)
          case None =>
            rubric match {
              case Some(r) => RubricGrader(() => solution().toHTML, r).grade(input)
              case None    => NotCheckable(solution().toHTML)
            }
        }

    /** points actually earned, rounded to the nearest half point */
    def awardedPoints(r: CheckResult): Double = Scoring.roundHalf(r.fraction * pts)

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
