# Codebase Guide

## Setup

Test the project by opening the `index.html` in a browser. For that to actually do something you first have to build everything using
```
sbt fastLinkJS
``` 

It will likely fail at first, because of a missing plugin. To fix that create a file at `project/plugins.sbt` and add the following line
```
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.19.0")
```

My SBT Version (sbt --version) is 1.10.10.

This is how I (Sungonogi) got everything to run, but there might be extra steps for things I already had installed.
In that case let me know / add to this README.


## Scala

Before looking at the code, here are the 5 core Scala concepts you need to know:

### 1. Classes vs. Objects vs. Case Classes
* **`class`**: A blueprint for creating objects (just like Java or Python).
* **`object`**: Declares a **Singleton** (a class that has exactly one instance). It is used for holding utility functions, constants, or the entry point (`main` method). Think of it like static methods in Java.
* **`case class`**: A class optimized for holding data. Scala automatically generates a constructor, fields, a readable `toString` method, value-based comparison (`==`), and a `.copy()` method for you.

### 2. Traits
A `trait` is like an interface in Java. It defines a list of methods that other classes must implement:
```scala
trait ProblemGenerator[PD] {
  def make(): PD // must be implemented by subclasses
}
```

### 3. Implicit Contexts
Scala has a unique feature called **context parameters**. 
If a method signature ends with `(using ctx: Context)` or `(implicit ctx: Context)`, it means you do not have to pass this argument explicitly if there is a matching context variable marked in the scope.
* **Providing the context**: `using myContext` or `implicit val ctx = ...`
* **Receiving the context**: `def evaluate(f: Form)(using ctx: Context)`

### 4. Functional Collections
Scala collections are typically manipulated using functional operations:
* **`map`**: Transforms every element in a list. E.g., `List(1, 2).map(x => x * 2)` $\to$ `List(2, 4)`.
* **`filter`**: Keeps elements matching a condition. E.g., `List(1, 2, 3).filter(x => x > 1)` $\to$ `List(2, 3)`.
* **`forall`**: Returns true if **all** elements match the condition.
* **`exists`**: Returns true if **at least one** element matches the condition.
* **`flatMap`**: Transforms elements and flattens nested lists. E.g., `List(1, 2).flatMap(x => List(x, x+1))` $\to$ `List(1, 2, 2, 3)`.

### 5. Pattern Matching
Pattern matching is like an advanced `switch` statement. It allows you to match an object against different structures or types and extract its inner fields directly:
```scala
expr match {
  case Var(name) => println(s"Variable name: $name")
  case Lit(value, domain) => println(s"Literal value: $value")
}
```

---

## `expression.scala`

This file implements the core mathematical and logical engine. It represents variables, numbers, operators, and logical formulas as syntax trees, and provides the functionality to programmatically evaluate them.

Here are some useful use cases:

#### 1. Creating Variables and Constants
In your problem generator, you define math elements using the following:
* **Integer Literals**: `DInt(5)` (represents the number $5$)
* **Boolean Literals**: `true` / `false`
* **Named Variables**: `Var("x")` (represents variable $x$)
* **Boolean Variables**: `BVar("a")` (represents boolean variable $a$)

#### 2. Building Arithmetic Expressions
You can build math trees using standard arithmetic symbols (Scala uses operator overloading to build these automatically):
```scala
val term = Var("x") + DInt(5)        // Represents: x + 5
val term2 = Var("y") * Var("x") - 3  // Represents: y * x - 3
```

#### 3. Building Logic and Comparisons
You can compare terms or build boolean logic formulas using custom operators:
* **Equality**: `Var("x") === 5` (maps to sTeX `\equals{x}{5}`)
* **Inequality**: `Var("x") =!= 5` (maps to sTeX `\nequals{x}{5}`)
* **Less than / Less-Equal**: `Less(Var("x"), 10)` or `LessEq(Var("x"), 10)`
* **Logical AND / OR / IMPLIES**: 
  ```scala
  // Represents: (x == 5) AND (y < 3)
  And(Var("x") === 5, Less(Var("y"), 3)) 
  
  // Represents: a => (b <= 2)
  Implies(BVar("a"), LessEq(Var("b"), 2))
  ```

#### 4. Evaluating Expressions 

To find the answer to a generated expression, you feed it to the `Evaluator` along with a `Context` containing current values:
```scala
// 1. Define the variable values:
val ctx = Context(List("x" -> 3, "y" -> 5))

// 2. Compute a math term:
val sum = Evaluator(Var("x") + Var("y"))(using ctx) // Returns: Lit(8, DInt)

// 3. Compute a logic formula:
val isTrue = Evaluator(Var("x") === 3)(using ctx) // Returns: true
```

#### 5. Converting to LaTeX (sTeX)
Any expression can be rendered directly to TeX code using `.toSTeX`:
```scala
val term = Var("x") === 5
println(term.toSTeX) // Outputs: \equals{x}{5}
```


## `generation.scala`

This file contains utility methods for generating random values and constructing random mathematical expression trees recursively.

Here are some use cases:

#### 1. Selecting Random Elements from a Collection
When generating random problem settings, you often need to select from a set of possible options:
* **Select a single random item**:
  ```scala
  // Picks one action randomly
  val randomAction = Generator.choose(List("up", "down", "left", "right"))
  ```
* **Select multiple random items**:
  ```scala
  // Picks 2 to 3 unique variables from the list
  val randomVars = Generator.chooseSome(List("a", "b", "c", "d"), atLeast = 2, atMost = 3, allowRep = false)
  ```

#### 2. Generating Basic Random Primatives
* **Flipping a coin (probability)**:
  ```scala
  // Returns true with 70% probability, false with 30%
  val isStochastic = Generator.chooseBoolean(0.7)
  ```
* **Choosing a random integer range**:
  ```scala
  // Picks a random integer between 3 and 5 (inclusive)
  val gridWidth = Generator.chooseInt(3, 5)
  ```

#### 3. Generating Random Mathematical Expression Trees
The file contains functions like `genTerm` and `genForm` to generate random syntax trees for exam questions (e.g. creating a random transition equation like `(s + a) % 5`).

It takes a `State` containing allowed variables, nesting depths, and operators. It recursively calls itself, deciding with a probability (based on current depth) whether to create a leaf node (a variable or literal number) or an operator node (like `Plus`, `Times`) with recursively generated sub-terms.

## `stex.scala`

This file bridges Scala objects with LaTeX/sTeX, defining document structure binders and a custom string interpolator to generate formatted LaTeX documents.

Here are some usages:

#### 1. LaTeX Document Environments
This file defines standard containers for exam sheets. Printing any of these objects (using `.toString`) automatically produces `\begin{env} ... \end{env}` text blocks:
* **`SDocument`**: The master container (includes `\documentclass{article}` and `\usepackage{stexlight}`).
* **`SProblem`**: Represents an exam question (`\begin{sproblem}`).
* **`SSubproblem`**: Represents a sub-question (`\begin{subproblem}[pts=3]`).
* **`SSolution`**: Wraps the solution (`\begin{solution}[testspace=4.0cm]`).

#### 2. Formatting Layouts
* **`SItemize` & `SEnumerate`**: Bulleted or numbered lists.
  ```scala
  SItemize(
    x"Variables: ${csp.variables}",
    x"Constraints: ..."
  )
  // Renders: \begin{itemize} \item Variables: ... \item Constraints: ... \end{itemize}
  ```
* **`STabular`**: Renders tabular grids (used for joint probability tables and state-action transition tables).

#### 3. The `x` String Interpolator
This is an implicit syntax wrapper added to Scala's string engine. It lets you write LaTeX text with variables cleanly:
* **Math Mode Switch**: Since Scala uses `$` for variable injection, **you must use `§` as the LaTeX math mode dollar sign** instead of `$`.
* **Auto-Typing**: Any variable inside `${}` is inspected and formatted automatically:
  * If it's a `Form` or `Term` (from `expression.scala`), it gets converted to LaTeX and wrapped in math-mode `$...$`.
  * If it's a `String` or another primitive, it gets printed as plain text.

Example:
```scala
val x = Var("x")
val text = x"Assume §$x = 5§. The goal state is ${mdp.goal}."
// Outputs: "Assume $x = 5$. The goal state is $\tup{2,2}$."
```

## `problem.scala`

This file defines the template structure and selection rules for building exam sheets. It implements traits and classes to manage modular subproblems and select them dynamically based on constraints.

Usage:

To create an exam topic, you extend `Problem` and declare inner `Subproblem` objects and `GroupConstraint`s:

#### 1. Defining the Problem Class
Extend the `Problem` trait and specify `intro()` (the preamble text of the exam question):
```scala
case class MyProblem(data: MyData) extends Problem[MyProblem] {
  override def intro(): SText = x"Consider the following system §S = ${data.name}§..."
}
```

#### 2. Creating Modular Subproblems
Inside your problem class, declare objects inheriting from `Subproblem`:
```scala
object subQuestion1 extends Subproblem(id = "theory", pts = 2, testspace = 3) {
  // Checks if this question is applicable for the current random data
  override def applicable(): Boolean = data.hasValidStates 
  
  // Runs right before printing (e.g. to pick a random state to ask about)
  override def init(): Unit = { ... } 
  
  // The LaTeX question text
  def question() = x"State the definition of..."
  
  // The LaTeX solution text
  def solution() = x"The definition is..."
}
```

#### 3. Defining Group Constraints
At the bottom of your problem class, use `GroupConstraint` to tell the generator how to compose the exam sheet:
```scala
// Forces the generator to pick exactly one of subQuestion1 or subQuestion2
GroupConstraint(atLeast = 1, atMost = 1, subQuestion1, subQuestion2)
```

#### 4. The Selection Lifecycle
When the generator runs, the system automatically resolves constraints:
1. `chooseSubproblems()` is called.
2. It filters out any subproblems where `applicable()` is false.
3. It checks `GroupConstraint`s, randomly discarding extra subproblems if they exceed `atMost`.
4. It calls `init()` on the remaining chosen subproblems to bind random variables.
5. It prints the selected questions via `.toSTeX(...)`.

## `main.scala`

This file is the entry point of the system. It orchestrates the lifecycle by generating problem parameters, selecting subproblems, and printing the formatted LaTeX output.

```scala
package info.kwarc.probgen

/** simple main method to call generators and see their results */
object Test {
  def main(args: Array[String]): Unit = {
    val p = MDPGenerator.make() // 1. Run generator search loop for an MDP problem
    val subs = p.chooseSubproblems() // 2. Filter & select questions
    val stex = p.toSTeX(subs) // 3. Render questions to sTeX
    if (args.nonEmpty && args(0) == "--full") {
      val d = SDocument("problem", stex) // 4a. Format as standalone document
      println(d.toStringFull)
    } else {
      println(stex) // 4b. Or print raw sTeX snippet
    }
  }
}
```

Run the generator using `sbt run`. Example output:

```
% --- Starting Generator ---
% Found good problem after 5 attempts. (pol. iter.: false, steps: 4)

%%%%%%%%%
\begin{sproblem}
Consider the following agent in a stochastic environment.
The agent moves in a $$3$ \times $3$$ grid.
Its possible actions are $\mathtt{up}$, $\mathtt{down}$, $\mathtt{right}$, $\mathtt{left}$.
These succeed with a probability of 0.6 and fail without movement otherwise.

The agent starts at $\tup{0,0}$. Its goal is to reach $\tup{2,2}$.
We use a reward of 10.0 for the goal and -2.0 otherwise, and a discount factor of $\equals{\gamma}{0.5}$.
We analyze this using value iteration.

\begin{subproblem}[pts={3}]
Give the sets of states $S$ and actions $A$ for this MDP.
\begin{solution}[testspace={5.0cm}]
$\equals{S}{\set{\seq{\tup{0,0},\tup{0,1},\tup{0,2},\tup{1,0},\tup{1,1},\tup{1,2},\tup{2,0},\tup{2,1},\tup{2,2}}}}$, $\equals{A}{\set{\seq{\mathtt{up},\mathtt{down},\mathtt{right},\mathtt{left}}}}$
\end{solution}
\end{subproblem}

\begin{subproblem}[pts={2}]
State the Bellman Optimality Equation for $\apply{U,s}$.
\begin{solution}[testspace={3.0cm}]
$\equals{\apply{U,s}}{\intplus{\apply{R,s},\inttimes{\gamma,\max_{\inset{a}{A}}{\sum_{\inset{s'}{S}}{\inttimes{\CondProb{s'}{s,\,a},\apply{U,s'}}}}}}}$
\end{solution}
\end{subproblem}

\begin{subproblem}[pts={3}]
Assume current utilities $\equals{\apply{U,s}}{0}$ for all states. Perform one value iteration step to calculate $\apply{U,\tup{0,2}}$.
\begin{solution}[testspace={4.0cm}]
With $\equals{U}{0}$, future rewards are $0$. Thus: $\equals{\apply{U,s}}{\apply{R,s}}{-2}$
\end{solution}
\end{subproblem}

\begin{subproblem}[pts={3}]
Suppose we solved the utilities as $\apply{U,s}$ for every state $s$. How do we derive the optimal policy?
\begin{solution}[testspace={4.0cm}]
Calculate the utilities for all actions and pick the max: $\equals{\apply{\pi,s}}{\argmax_{\inset{s'}{S}}{\inttimes{\CondProb{s'}{s,\,a},\apply{U,s'}}}}$.
\end{solution}
\end{subproblem}

\begin{subproblem}[pts={2}]
Calculate the probability of the agent moving along to $\tup{0,0}$ and then to $\tup{1,0}$by taking the actions \mathtt{down} and \mathtt{right}.
\begin{solution}[testspace={3.0cm}]
\[ P = 1.0 \times 0.6 = 0.6 \]
\end{solution}
\end{subproblem}

\begin{subproblem}[pts={3}]
Now assume we use the corresponding POMDP with current belief state $\equals{\apply{b,\tup{0,0}}}{0.5}$, $\equals{\apply{b,\tup{0,1}}}{0.5}$ and apply action $\equals{a}{\tup{right,\tup{1,0}}}$. Calculate the new belief state.
\begin{solution}[testspace={4.0cm}]
$\equals{\apply{b',\tup{1,0}}}{0.3}$, $\equals{\apply{b',\tup{0,0}}}{0.2}$, $\equals{\apply{b',\tup{1,1}}}{0.3}$, $\equals{\apply{b',\tup{0,1}}}{0.2}$
\end{solution}
\end{subproblem}
\end{sproblem}
```

