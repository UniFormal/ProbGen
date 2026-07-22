# CSP Problem Generator

We generate a CSP problem using the following structure:

* **Variables**: Fixed to $V = \{a, b, c, d\}$, where $a$ is boolean and $b, c, d$ are integers.
* **Domain Pools**: 
  * $D_a = \{0, 1\}$ (representing `{false, true}`)
  * $D_b = \{0, 1, 2, 3\}$
  * $D_c = \{0, 1, 2, 3\}$
  * $D_d = \{0, 1, 2, 3, 4, 5, 6\}$
* **Constraint Layout**: The topology of which variables relate to each other:
  1. $a \implies b \leq k_1$ ($k_1$ is randomly selected from $\{1, 2, 3\}$)
  2. $c < 2 \implies a$
  3. $b + c < k_3$ ($k_3$ is randomly selected from $\{3, 4, 5\}$)
  4. $d < b$
  5. $d = f(c)$: where $f$ is randomly chosen to be either multiplication ($d=2c$) or addition ($d = c + k_5$) for $k_5 \in \{1, 2, 3\}$
* **Forward Checking Assignment**: Always starts by assigning $1$ ($true$) to $a$.



## `logic.scala`

This file implements the mathematical representation and algorithms for the CSP domain.


#### 1. The `CSP` Structure
A CSP Problem is $\left\langle V,D,C \right\rangle$

```scala
case class CSP(
  variables: List[String],
  domains: Map[String, List[Int]],
  constraints: List[Form]
)
```


#### 2. The `toContext` Converter
```scala
def toContext(pairs: List[(String, Int)]): Context = {
  Context(pairs.map {
    case ("a", v) => "a" -> (v == 1)
    case (k, v) => k -> v
  })
}
```
This translates the solver's integer representations back to Booleans for variable `"a"` (mapping `1` to `true` and `0` to `false`) while keeping other variables as integers. It wraps the result in a `Context` for the math engine.

#### 3. Variable Crawler
```scala
def variablesIn(e: Expr): Set[String] = e match {
  case Var(n) => Set(n)
  case BVar(n) => Set(n)
  case Lit(_, _) => Set.empty
  case Pred(_, args) => args.flatMap(variablesIn).toSet
  case Conn(_, args) => args.flatMap(variablesIn).toSet
  case Apply(_, args) => args.flatMap(variablesIn).toSet
  case _ => Set.empty
}
```
This inspects an expression tree recursively (using pattern matching) to find all variable names involved in it.

#### 4. Consistency Checker 
```scala
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
```
This checks if a partial assignment is consistent. It only evaluates constraints where all variables have been assigned. If a constraint evaluates to `false`, it returns `false`.

#### 5. Backtracking Search

Uses Depth-First Search (DFS) backtracking to find all solutions. If a partial assignment violates any constraint, it prunes that search branch early to save computation time.

```scala
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
```


#### 6. Inconsistent Assignment Finder

Computes the Cartesian product of all domains to get all possible total variable assignments, and returns the first one that is not in the set of solutions.

```scala
def findInconsistentAssignment(): Option[Map[String, Int]] = {
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
```

#### 7. Forward Checking Step 

Simulates the forward-checking step. It assumes `assignedVar` is set to `value`, and filters the domains of all other variables to remove values that violate any binary constraints between them.

```scala
def forwardChecking(assignedVar: String, value: Int): Map[String, List[Int]] = {
  var newDomains = domains
  newDomains = newDomains + (assignedVar -> List(value))

  val otherVars = variables.filter(_ != assignedVar)
  otherVars.foreach { y =>
    val currentDomainY = domains(y)
    val filteredDomainY = currentDomainY.filter { valY =>
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
```

#### 8. Arc Consistency Checker

Verifies if $v$ is arc-consistent relative to $w$. It checks that for **every** value in $D_v$, there is **at least one** value in $D_w$ such that all constraints between $v$ and $w$ are satisfied.

```scala
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
```

## `problem.scala`

This file handles the presentation layout of the CSP problem in LaTeX/sTeX.

### Introducing the problem

```scala
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
}
```


### Subproblem 1: Find All Solutions

Defines a 3-point question. The answer block triggers `csp.solve()` and lists all coordinate values cleanly (e.g. `a = 1, b = 2, c = 0, d = 1`).

```scala
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
```

#### Subproblem 2: Inconsistent Assignment

Defines a 1-point question. The answer searches for any assignment that violates at least one constraint and prints it out.

```scala
object giveInconsistentAssignment extends Subproblem("inconsistent", 1, 2) {
  def question() = x"Give an inconsistent total variable assignment."
  def solution() = {
    csp.findInconsistentAssignment() match {
      case Some(as) =>
        val asStr = as.toList.sortBy(_._1).map { case (k, v) => s"$k = $v" }.mkString(", ")
        x"Any assignment that is not a solution, e.g., $asStr."
      case None =>
        x"No inconsistent total assignment exists."
    }
  }
}
```

#### Subproblem 3: Arc-Consistency Checkboxes

This randomly selects 4 pairs of variables, outputs a checkbox list `\begin{mcb}`, and calculates whether each pair is arc-consistent using `csp.isArcConsistent(v, w)` and marks the box as true (`\mcc[T]`) or false (`\mcc[F]`).

```scala
object checkArcConsistency extends Subproblem("ac", 2, 3) {
  var testPairs: List[(String, String)] = Nil

  override def init(): Unit = {
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
```

#### Subproblem 4: Forward Checking Step

Asks the student to perform a single step of forward checking starting from `assignedVar = assignedVal` and outputs the resulting filtered domains.

```scala
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
```

#### Group Constraints

Forces the exam sheet selector to include exactly one instance of each of these subproblems on the sheet.

```scala
GroupConstraint(1, 1, giveSolutions)
GroupConstraint(1, 1, giveInconsistentAssignment)
GroupConstraint(1, 1, checkArcConsistency)
GroupConstraint(1, 1, forwardChecking)
```

## `generator.scala`

This file implements the search-and-filter loop that randomly constructs constraint parameters and selects valid CSP problem.

#### 1. Generator Object
Inherits `ProblemGenerator[CSPProblem]`. It enters an infinite loop, increments a counter, and generates random parameter boundaries.

```scala
object CSPGenerator extends ProblemGenerator[CSPProblem] {
  def make(): CSPProblem = {
    var attempts = 0
    println("% --- Starting CSP Generator ---")
    while (true) {
      attempts += 1
      // 1. Pick random values
      val k1 = Generator.choose(List(1, 2, 3))
      val k3 = Generator.choose(List(3, 4, 5))
      val useMult = Generator.chooseBoolean(0.5)
      val k5 = Generator.choose(List(1, 2, 3))
      ...
```

#### 2. Building Constraints & Solving

* Constructs the final constraint `c5` (either $d = 2c$ or $d = c + k_5$).
* Wraps the variables, static domains, and dynamic constraints list inside a new `CSP` instance.
* Solves the system using backtracking (`csp.solve()`).

```scala
      // 2. Build the dynamic final relation constraint
      val c5 = if (useMult) {
        Equals(Var("d"), Times(2, Var("c")))
      } else {
        Equals(Var("d"), Plus(Var("c"), k5))
      }

      val domains = Map(
        "a" -> List(0, 1),
        "b" -> List(0, 1, 2, 3),
        "c" -> List(0, 1, 2, 3),
        "d" -> List(0, 1, 2, 3, 4, 5, 6)
      )

      val constraints = List(
        Implies(BVar("a"), LessEq(Var("b"), k1)),
        Implies(Less(Var("c"), 2), BVar("a")),
        Less(Plus(Var("b"), Var("c")), k3),
        Less(Var("d"), Var("b")),
        c5
      )

      val csp = CSP(List("a", "b", "c", "d"), domains, constraints)
      val solutions = csp.solve()
```


#### 3. Filtering Candidates

Checks if the problem satisfies the criteria for an ideal exam sheet:
  1. It must be solvable (`solutions.nonEmpty`).
  2. The number of solutions must be small (`solutions.length <= 4`).
  3. There must exist at least one inconsistent total assignment (`findInconsistentAssignment().isDefined`).
  
  If all criteria are met, it breaks the loop and returns the initialized `CSPProblem`. Otherwise, it discards the configuration and loops to try again.

```scala
      if (solutions.nonEmpty && solutions.length <= 4 && csp.findInconsistentAssignment().isDefined) {
        println(s"% Found good CSP problem after $attempts attempts. (solutions: ${solutions.length})")
        return CSPProblem(csp, "a", 1)
      } else {
        // Discard and run the loop again
      }
```
