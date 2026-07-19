package info.kwarc.probgen

import SText._
import Expr._

/**
  * A generic Constraint Satisfaction Problem (CSP)
  */
case class CSP(
  variables: List[String],
  domains: Map[String, List[Int]],
  constraints: List[Form]
) {

  /**
    * Convert a list of integer assignments to a Context, mapping "a" to a Boolean.
    */
  def toContext(pairs: List[(String, Int)]): Context = {
    Context(pairs.map {
      case ("a", v) => "a" -> (v == 1)
      case (k, v) => k -> v
    })
  }

  /**
    * Recursively finds all variable names in an expression.
    */
  def variablesIn(e: Expr): Set[String] = e match {
    case Var(n) => Set(n)
    case BVar(n) => Set(n)
    case Lit(_, _) => Set.empty
    case Pred(_, args) => args.flatMap(variablesIn).toSet
    case Conn(_, args) => args.flatMap(variablesIn).toSet
    case Apply(_, args) => args.flatMap(variablesIn).toSet
    case _ => Set.empty
  }

  /**
    * Checks if a partial assignment is consistent with all constraints.
    * A constraint is checked only if all variables in its scope are assigned.
    */
  def isConsistent(assignment: Map[String, Int]): Boolean = {
    constraints.forall { f =>
      val vars = variablesIn(f)
      if (vars.forall(assignment.contains)) {
        try {
          Evaluator(f)(using toContext(assignment.toList))
        } catch {
          case _: Exception => false
        }
      } else {
        true
      }
    }
  }

  /**
    * Backtracking search to find all complete, consistent assignments.
    */
  def solve(): List[Map[String, Int]] = {
    def backtrack(assignment: Map[String, Int], unassigned: List[String]): List[Map[String, Int]] = {
      if (unassigned.isEmpty) {
        if (isConsistent(assignment)) List(assignment) else Nil
      } else {
        val v = unassigned.head
        domains(v).flatMap { valVal =>
          val nextAssignment = assignment + (v -> valVal)
          if (isConsistent(nextAssignment)) {
            backtrack(nextAssignment, unassigned.tail)
          } else {
            Nil
          }
        }
      }
    }
    backtrack(Map.empty, variables)
  }

  /**
    * Finds one inconsistent total assignment.
    */
  def findInconsistentAssignment(): Option[Map[String, Int]] = {
    // Generate the cartesian product of domains
    def cartesianProduct(vars: List[String]): List[Map[String, Int]] = vars match {
      case Nil => List(Map.empty)
      case v :: tail =>
        for {
          valVal <- domains(v)
          rest <- cartesianProduct(tail)
        } yield rest + (v -> valVal)
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
      val filteredDomainY = currentDomainY.filter { valY =>
        // Check if this value is consistent under constraints involving only assignedVar and y
        constraints.forall { f =>
          val vars = variablesIn(f)
          if (vars.contains(assignedVar) && vars.contains(y) && vars.forall(v => v == assignedVar || v == y)) {
            try {
              Evaluator(f)(using toContext(List(assignedVar -> value, y -> valY)))
            } catch {
              case _: Exception => false
            }
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
      val vars = variablesIn(f)
      vars.contains(v) && vars.contains(w) && vars.forall(x => x == v || x == w)
    }

    if (relevantConstraints.isEmpty) return true

    domains(v).forall { valV =>
      domains(w).exists { valW =>
        relevantConstraints.forall { f =>
          try {
            Evaluator(f)(using toContext(List(v -> valV, w -> valW)))
          } catch {
            case _: Exception => false
          }
        }
      }
    }
  }
}
