package info.kwarc.probgen

import SText._
import Expr._

case class CSPProblem(
  csp: CSP,
  assignedVar: String,
  assignedVal: Int
) extends Problem[CSPProblem] {

  override def intro(): SText = {
    val varsList = csp.variables.mkString(", ")
    val domainsList = csp.variables.map { v =>
      val domVals = csp.domains(v).mkString(", ")
      s"D_{$v} = \\{$domVals\\}"
    }.mkString(", ")

    val constraintsList = SItemize(csp.constraints.map(c => x"${c}")*)

    SSnippet(List(
      x"Consider the following constraint network §\\langle V, D, C \\rangle§:",
      SItemize(
        x"Variables §V = \\{$varsList\\}§",
        x"Domains §$domainsList§",
        x"Constraints §C§:"
      ),
      constraintsList
    ), "\n")
  }

  // --- SUBPROBLEMS ---

  // 1. Give all solutions
  object giveSolutions extends Subproblem("solutions", 3, 4) {
    def question() = x"Give all solutions."
    def solution() = {
      val sols = csp.solve()
      if (sols.isEmpty) {
        x"There are no solutions."
      } else {
        val solStrings = sols.map { sol =>
          sol.toList.sortBy(_._1).map { case (k, v) => s"$k = $v" }.mkString(", ")
        }.mkString("; ")
        x"The solutions are: $solStrings."
      }
    }
  }

  // 2. Give an inconsistent assignment
  object giveInconsistentAssignment extends Subproblem("inconsistent", 1, 2) {
    def question() = x"Give an inconsistent total variable assignment."
    def solution() = {
      csp.findInconsistentAssignment() match {
        case Some(as) =>
          val asStr = as.toList.sortBy(_._1).map { case (k, v) => s"$k = $v" }.mkString(", ")
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

    def question() = {
      val items = testPairs.map { case (v, w) =>
        s"\\mcc{(\\text{arc-consistent: } $v \\text{ relative to } $w)}"
      }.mkString("\n")
      
      x"Check the boxes for pairs §(v, w)§ where §v§ is arc-consistent relative to §w§." +
      x"""
\begin{mcb}
$items
\end{mcb}"""
    }

    def solution() = {
      val items = testPairs.map { case (v, w) =>
        val isAc = csp.isArcConsistent(v, w)
        val tag = if (isAc) "T" else "F"
        s"\\mcc[$tag]{(\\text{arc-consistent: } $v \\text{ relative to } $w)}"
      }.mkString("\n")

      x"""
\begin{mcb}
$items
\end{mcb}"""
    }
  }

  // 4. Forward Checking
  object forwardChecking extends Subproblem("fc", 2, 3) {
    def question() = {
      x"Assume a CSP algorithm starts by assigning $assignedVal to $assignedVar." +
      x" How do the domains of the remaining variables change after forward checking?"
    }

    def solution() = {
      val newDoms = csp.forwardChecking(assignedVar, assignedVal)
      val otherDoms = csp.variables.filter(_ != assignedVar).map { v =>
        val domVals = newDoms(v).mkString(", ")
        s"D_{$v} = \\{$domVals\\}"
      }.mkString(", ")
      x"The updated domains are: $otherDoms."
    }
  }

  // --- CONSTRAINTS ---
  GroupConstraint(1, 1, giveSolutions)
  GroupConstraint(1, 1, giveInconsistentAssignment)
  GroupConstraint(1, 1, checkArcConsistency)
  GroupConstraint(1, 1, forwardChecking)
}
