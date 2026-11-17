package info.kwarc.probgen

import SText._
import Expr._

case class CSPProblem(csp: CSP, assignedVar: String, assignedVal: Int) extends Problem[CSPProblem] {

  /** the domains of some variables, e.g. D_a = {0, 1}, D_b = {0, 1, 2} */
  private def domainsText(doms: Map[String, List[Int]], vars: List[String]): SText =
    SSnippet(vars.map(v => SMath(Var(s"D_$v") === FinSet(doms(v)))), ", ")

  override def intro(): SText = {
    val constraintsList = SItemize(csp.constraints.map(c => x"${c}")*)

    SSnippet(List(
      x"Consider the following constraint network ${Tuple(Var("V"),Var("D"), Var("C"))}:",
      SItemize(
        x"Variables ${Var("V") === FinSet(csp.variables.map(Var(_))*)}",
        x"Domains ${domainsText(csp.domains, csp.variables)}",
        x"Constraints ${Var("C")}:"
      ),
      constraintsList
    ), "\n")
  }

  /** a student's assignment, if it gives every variable a value from its domain */
  private def totalAssignment(c: Context): Either[String, Context] =
    if (c.asMap.keySet != csp.variables.toSet)
      Left(s"Give each of ${Html.codeList(csp.variables)} exactly one value.")
    else Grader.sequence(csp.variables) { v =>
      ExprCompare.numericValue(c(v)) match {
        case Some(x) if x.isWhole && csp.domains(v).contains(x.toInt) => Right(v -> DInt(x.toInt))
        case _ => Left(s"The value of <code>$v</code> must be in its domain {${csp.domains(v).mkString(", ")}}.")
      }
    }.map(Context(_))

  // --- SUBPROBLEMS ---

  // 1. Give all solutions
  object giveSolutions extends Subproblem("solutions", 3, 4) {
    override def answerHint = "e.g. a=1, b=0, c=2, d=4; a=1, b=2, c=0, d=1"
    override def grader = Some(ContextSetGrader(csp.solve(), csp.variables.toSet))
    def question() = x"Give all solutions."
    def solution() = {
      val sols = csp.solve()
      if (sols.isEmpty) {
        x"There are no solutions."
      } else {
        val solStrings = sols.mkString("; ")
        x"The solutions are: $solStrings."
      }
    }
  }

  // 2. Give an inconsistent assignment
  object giveInconsistentAssignment extends Subproblem("inconsistent", 1, 2) {
    override def answerHint = "e.g. a=0, b=0, c=0, d=0"
    // many answers are right, so the constraints decide
    override def grader = Some(VerifiedGrader[Context](
      input => ExprParser.parseContext(input).flatMap(totalAssignment),
      assignment => !csp.isConsistent(assignment),
      "This assignment satisfies every constraint, so it is a solution, not an inconsistent assignment."))
    def question() = x"Give an inconsistent total variable assignment."
    def solution() = {
      csp.findInconsistentAssignment() match {
        case Some(as) =>
          val asStr = as.toString
          x"Any assignment that is not a solution, e.g., $asStr."
        case None =>
          x"No inconsistent total assignment exists (all assignments are solutions)."
      }
    }
  }

  // 3. Check Arc-Consistency
  object checkArcConsistency extends Subproblem("ac", 2, 3) {
    // We choose 4 pairs of variables to ask the student about
    var testPairs: List[(String, String)] = Nil

    override def init(): Unit = {
      // Pick 4 distinct pairs of variables
      val allPairs = for {
        v <- csp.variables
        w <- csp.variables
        if v != w
      } yield (v, w)
      testPairs = Generator.chooseSome(allPairs, 4, 4, false)
    }

    override def answerHint = "T or F for each box, in order, e.g. T, F, F, T"
    override def grader = Some(BoolSeqGrader(testPairs.map { case (v, w) => csp.isArcConsistent(v, w) }))

    private def choice(v: String, w: String): SText =
      x"${Var(v)} is arc-consistent relative to ${Var(w)}"

    def question() =
      x"Check the boxes for pairs ${Tuple(Var("v"), Var("w"))} where ${Var("v")} is arc-consistent relative to ${Var("w")}.\n" +
        SChoices(testPairs.map { case (v, w) => (choice(v, w), None) })

    def solution() =
      SChoices(testPairs.map { case (v, w) => (choice(v, w), Some(csp.isArcConsistent(v, w))) })
  }

  // 4. Forward Checking
  object forwardChecking extends Subproblem("fc", 2, 3) {
    override def answerHint = "e.g. D_b = {0, 1}, D_c = {2, 3}, D_d = {4}"
    override def grader = {
      val newDoms = csp.forwardChecking(assignedVar, assignedVal)
      val remaining = csp.variables.filter(_ != assignedVar)
      Some(EquationsGrader(remaining.map(v => (Var(s"D_$v"): Expr) -> FinSet(newDoms(v)))))
    }
    def question() = {
      x"Assume a CSP algorithm starts by assigning $assignedVal to ${Var(assignedVar)}." +
      x" How do the domains of the remaining variables change after forward checking?"
    }

    def solution() = {
      val newDoms = csp.forwardChecking(assignedVar, assignedVal)
      x"The updated domains are: ${domainsText(newDoms, csp.variables.filter(_ != assignedVar))}."
    }
  }

  // --- CONSTRAINTS ---
  GroupConstraint(1, 1, giveSolutions)
  GroupConstraint(1, 1, giveInconsistentAssignment)
  GroupConstraint(1, 1, checkArcConsistency)
  GroupConstraint(1, 1, forwardChecking)
}
