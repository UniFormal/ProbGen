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
      * Override to have this subproblem graded automatically. See answers.scala for the available graders. 
    */
    def grader: Option[Grader] = None

    /**
      * Override to attach a grading scheme to a question that cannot be graded automatically.
      */
    def rubric: Option[AnswerRubric] = None

    /**
      * The entry point for grading an answer; callers use this, not `checkSolution`.
      * It handles what every question has in common and then delegates. It is
      * `final`, so subproblems that override `checkSolution` cannot skip it.
      */
    final def check(input: String): CheckResult = {
      val trimmed = input.trim
      if (trimmed.isEmpty) MalformedInput("Please enter an answer first.")
      else checkSolution(trimmed)
    }

    /**
      * A subproblem is graded when it says how, via `grader` (see answers.scala) or `rubric`, and otherwise question is simply reported as not auto-graded.
      */
    def checkSolution(input: String): CheckResult =
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
    groupConstraints = groupConstraints :+ this
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
