package info.kwarc.probgen

import SText.*

/** A propositional-logic exercise built around one randomly generated
  * formula: convert it to CNF, convert it to DNF, and decide whether it is
  * equivalent to some other formula. Mirrors the [[CSPProblem]]/
  * [[BasicProbabilityProblem]] shape: one `intro()` plus a handful of
  * `Subproblem`s, each generating/grading itself.
  */
case class PropLogicProblem(formula: Form) extends Problem[PropLogicProblem] {

  private def vars = PropLogic.collectVars(formula).toList.sorted

  def intro(): SText =
    x"Consider the propositional formula $formula over the variables ${vars}." +
      x" Formulas are written using 'and', 'or', 'neg', '->' (or 'implies') and parentheses, e.g. 'neg p or (q and r)'."

  // Grade by parsing the student's answer and checking it is both
  // structurally in the right normal form and equivalent to the original -
  // any valid CNF/DNF is accepted, not just the one particular shape toCNF/
  // toDNF happens to produce.
  private def checkNormalForm(
      input: String,
      isRightForm: Form => Boolean,
      formName: String
  ): CheckResult =
    try
      val userForm = PropLogic.parseForm(input)
      if !isRightForm(userForm) then
        Incorrect(s"That is not in $formName - check the shape of your formula.")
      else if !PropLogic.isEquivalent(userForm, formula) then
        Incorrect(s"That $formName is not equivalent to the original formula.")
      else Correct()
    catch case e: Exception => Incorrect(s"Could not parse your formula: ${e.getMessage}")

  object ConvertToCNF extends Subproblem("cnf", 2, 3) {
    def question() = x"Convert $formula to Conjunctive Normal Form (CNF)."
    def solution() = x"${PropLogic.toCNF(formula)}"
    override def checkSolution(input: String): CheckResult =
      checkNormalForm(input, PropLogic.isCNF, "CNF")
  }

  object ConvertToDNF extends Subproblem("dnf", 2, 3) {
    def question() = x"Convert $formula to Disjunctive Normal Form (DNF)."
    def solution() = x"${PropLogic.toDNF(formula)}"
    override def checkSolution(input: String): CheckResult =
      checkNormalForm(input, PropLogic.isDNF, "DNF")
  }

  object CheckEquivalence extends Subproblem("equiv", 1, 1) {
    var other: Form = null
    var equivalent: Boolean = false

    override def init(): Unit =
      // half the time show an equivalent (but differently-shaped) formula,
      // half the time a genuinely different random one over the same vars
      other =
        if Generator.chooseBoolean(0.5) then PropLogic.toCNF(formula)
        else PropFormulaGenerator.generate(vars = vars, minDepth = 1, maxDepth = 3, minVars = 1)
      equivalent = PropLogic.isEquivalent(formula, other)

    def question() =
      x"Are the formulas $formula and $other logically equivalent? Answer yes or no."
    def solution() = x"${if equivalent then "yes" else "no"}"

    override def checkSolution(input: String): CheckResult =
      val ans = input.trim.toLowerCase
      val saysYes = Set("yes", "y", "true", "equivalent").contains(ans)
      val saysNo = Set("no", "n", "false", "not equivalent", "notequivalent").contains(ans)
      if !saysYes && !saysNo then NotCheckable(solution().toText)
      else if saysYes == equivalent then Correct()
      else Incorrect(s"The correct answer is: ${if equivalent then "yes" else "no"}.")
  }

  GroupConstraint(1, 1, ConvertToCNF)
  GroupConstraint(1, 1, ConvertToDNF)
  GroupConstraint(1, 1, CheckEquivalence)
}

/** Randomly generates a [[PropLogicProblem]], analogous to
  * [[SearchProblemGenerator]]/[[CSPGenerator]]: pick generation parameters,
  * build a formula with [[PropFormulaGenerator]], wrap it in a problem.
  */
object LogicProblemGenerator extends ProblemGenerator[PropLogicProblem] {
  val vars = List("p", "q", "r")
  val minDepth = 2
  val maxDepth = 4
  val minVars = 2
  val weights = ConnectiveWeights(and = 30, or = 30, implies = 30, not = 10)

  def make(): PropLogicProblem =
    val formula = PropFormulaGenerator.generate(vars, minDepth, maxDepth, minVars, weights)
    log("chosen formula: " + formula.toText)
    PropLogicProblem(formula)
}
