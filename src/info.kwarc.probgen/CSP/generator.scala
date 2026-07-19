package info.kwarc.probgen

import SText._
import Expr._

object CSPGenerator extends ProblemGenerator[CSPProblem] {

  def make(): CSPProblem = {
    var good = false
    var problem: CSPProblem = null
    var attempts = 0

    log("--- Starting CSP Generator ---")

    val variables = List("a", "b", "c", "d")
    val domains = Map(
      "a" -> List(0, 1),
      "b" -> List(0, 1, 2, 3),
      "c" -> List(0, 1, 2, 3),
      "d" -> List(0, 1, 2, 3, 4, 5, 6)
    )

    while (!good) {
      attempts += 1

      // Random parameters to ensure variety
      val k1 = Generator.choose(List(1, 2, 3))
      val k3 = Generator.choose(List(3, 4, 5))
      val useMult = Generator.chooseBoolean(0.5)

      val c5 = if (useMult) {
        Equals(Var("d"), Times(2, Var("c")))
      } else {
        val k5 = Generator.choose(List(1, 2, 3))
        Equals(Var("d"), Plus(Var("c"), k5))
      }

      val constraints = List(
        Implies(BVar("a"), LessEq(Var("b"), k1)),
        Implies(Less(Var("c"), 2), BVar("a")),
        Less(Plus(Var("b"), Var("c")), k3),
        Less(Var("d"), Var("b")),
        c5
      )

      val csp = CSP(variables, domains, constraints)
      val solutions = csp.solve()

      // We want a solvable problem, but with few solutions (1 to 4 is ideal)
      if (solutions.nonEmpty && solutions.length <= 4 && csp.findInconsistentAssignment().isDefined) {
        log(s"Found good CSP problem after $attempts attempts. (solutions: ${solutions.length})")
        
        // Pick "a" as the starter assigned variable, assigning 1 (true)
        problem = CSPProblem(csp, "a", 1)
        good = true
      }
    }
    problem
  }
}
