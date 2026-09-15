package info.kwarc.probgen

import SText._
import Expr._

/**
  * A generic Constraint Satisfaction Problem (CSP)
  */
case class CSP(variables: List[String],domains: Map[String, List[Int]],constraints: List[Form]) {
  /**
    * Checks if a partial assignment is consistent with all constraints.
    * A constraint is checked only if all variables in its scope are assigned.
    */
  def isConsistent(assignment: Context): Boolean = {
    constraints.forall {f =>
      if (f.fvs.forall(assignment.declares)) {
        Evaluator(f)(using assignment)
      } else {
        true
      }
    }
  }

  /** backtracking search to find all complete, consistent assignments. */
  def solve(): List[Context] = {
    def backtrack(assignment: Context, unassigned: List[String]): List[Context] = {
      if (unassigned.isEmpty) {
        if (isConsistent(assignment)) List(assignment) else Nil
      } else {
        val v = unassigned.head
        domains(v).flatMap {valVal =>
          val nextAssignment = assignment(v -> valVal)
          if (isConsistent(nextAssignment)) {
            backtrack(nextAssignment, unassigned.tail)
          } else {
            Nil
          }
        }
      }
    }
    backtrack(Context(), variables)
  }

  /**
    * Finds one inconsistent total assignment.
    */
  def findInconsistentAssignment(): Option[Context] = {
    // Generate the cartesian product of domains
    def cartesianProduct(vars: List[String]): List[Context] = vars match {
      case Nil => List(Context())
      case v :: tail =>
        for {
          valVal <- domains(v)
          rest <- cartesianProduct(tail)
        } yield rest(v -> valVal)
    }
    val allAssignments = cartesianProduct(variables)
    val solutions = solve()
    allAssignments.find(a => !solutions.contains(a))
  }

  /**
    * Performs a single step of forward checking on unassigned variables
    * after assigning `assignedVar = value`.
    */
  def forwardChecking(assignedVar: String, value: Int): Map[String, List[Int]] = {
    var newDomains = domains
    newDomains = newDomains + (assignedVar -> List(value))
    val otherVars = variables.filter(_ != assignedVar)
    otherVars.foreach { y =>
      val currentDomainY = domains(y)
      val filteredDomainY = currentDomainY.filter {valY =>
        // Check if this value is consistent under constraints involving only assignedVar and y
        constraints.forall {f =>
          val vars = f.fvs
          if (vars.contains(assignedVar) && vars.contains(y) && vars.forall(v => v == assignedVar || v == y)) {
            Evaluator(f)(using Context(assignedVar -> value)(y -> valY))
          } else {
            true
          }
        }
      }
      newDomains = newDomains + (y -> filteredDomainY)
    }
    newDomains
  }

  /**
    * Checks if variable `v` is arc-consistent relative to variable `w`.
    * This means for every value in D_v, there is at least one value in D_w
    * that satisfies the constraints between them.
    */
  def isArcConsistent(v: String, w: String): Boolean = {
    val relevantConstraints = constraints.filter { f =>
      val vars = f.fvs
      vars.contains(v) && vars.contains(w) && vars.forall(x => x == v || x == w)
    }
    if (relevantConstraints.isEmpty) return true
    domains(v).forall {valV =>
      domains(w).exists {valW =>
        relevantConstraints.forall {f =>
          Evaluator(f)(using Context(v -> valV)(w -> valW))
        }
      }
    }
  }
}
