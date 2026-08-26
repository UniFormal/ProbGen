package info.kwarc.probgen

import SText.*

/** A propositional-logic exercise: convert the main formula to CNF and to
  * DNF, decide whether it's equivalent to another formula, and - each on
  * its own freshly generated formula - list satisfying assignments, list
  * falsifying assignments, and decide validity. Mirrors the [[CSPProblem]]/
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

  // All 2^|fvars| assignments of a formula over fvars, each a List[(var,value)]
  // aligned to fvars.
  private def allAssignments(fvars: List[String]): List[List[(String, Boolean)]] =
    PropLogic.efficientAssignment(fvars.length).map(bits => fvars.zip(bits.toList)).toList

  private def holds(f: Form, assignment: List[(String, Boolean)]): Boolean =
    Evaluator(f)(using Context(assignment))

  private def formatAssignment(assignment: List[(String, Boolean)]): String =
    assignment.map { case (v, b) => s"$v = $b" }.mkString(", ")

  // Parse "p = true, q = false" into a var->value set, order-independent, or
  // None if malformed or it doesn't mention exactly the given variables.
  private def parseAssignment(fvars: List[String], s: String): Option[Set[(String, Boolean)]] =
    val pairs = s.split(",").toList.map(_.trim).filter(_.nonEmpty).map(_.split("=").toList.map(_.trim))
    if pairs.exists(p => p.length != 2 || !(p(1).equalsIgnoreCase("true") || p(1).equalsIgnoreCase("false")))
    then None
    else
      val set = pairs.map(p => (p(0), p(1).equalsIgnoreCase("true"))).toSet
      if set.map(_._1) == fvars.toSet then Some(set) else None

  // Parse a ';'-separated list of assignments ("none"/empty means no assignments).
  private def parseAssignmentList(fvars: List[String], s: String): Option[Set[Set[(String, Boolean)]]] =
    val trimmed = s.trim
    if trimmed.isEmpty || trimmed.equalsIgnoreCase("none") then Some(Set.empty)
    else
      val parsed = trimmed.split(";").toList.map(_.trim).filter(_.nonEmpty).map(parseAssignment(fvars, _))
      if parsed.forall(_.isDefined) then Some(parsed.flatten.toSet) else None

  private def parseYesNo(input: String): Option[Boolean] =
    val ans = input.trim.toLowerCase
    if Set("yes", "y", "true").contains(ans) then Some(true)
    else if Set("no", "n", "false").contains(ans) then Some(false)
    else None

  // Shared shape for "list all satisfying/falsifying assignments of ..." -
  // each instance generates its own fresh formula (distinct from `formula`
  // and from each other), so seeing one doesn't spoil the others.
  abstract class AssignmentSubproblem(id: String, satisfying: Boolean)
      extends Subproblem(id, 2, 4) {
    private val kind = if satisfying then "satisfying" else "falsifying"
    var ownFormula: Form = null
    private def ownVars = PropLogic.collectVars(ownFormula).toList.sorted

    override def init(): Unit =
      ownFormula = PropFormulaGenerator.generate(vars = vars, minDepth = 2, maxDepth = 4, minVars = 2)

    private def matching: List[List[(String, Boolean)]] =
      allAssignments(ownVars).filter(a => holds(ownFormula, a) == satisfying)

    def question() =
      x"List all $kind assignments of $ownFormula - every assignment that makes it" +
        x" ${if satisfying then "true" else "false"}." +
        x" Write each as e.g. '${formatAssignment(ownVars.map(v => (v, true)))}', separate" +
        x" several with ';', and write 'none' if there are none."

    def solution(): SText =
      val ms = matching
      if ms.isEmpty then x"There are no $kind assignments."
      else SSnippet(List(SItemize(ms.map(a => SPlainText(formatAssignment(a)))*)))

    override def checkSolution(input: String): CheckResult =
      parseAssignmentList(ownVars, input) match
        case Some(userAns) =>
          val expected = matching.map(_.toSet).toSet
          if userAns == expected then Correct() else Incorrect(s"Not quite - expected: ${solution().toText}")
        case None =>
          Incorrect("Could not parse your answer - use 'var = true/false' pairs separated by commas, and ';' between assignments.")
  }

  object GiveSatisfyingAssignments extends AssignmentSubproblem("sat", satisfying = true)
  object GiveFalsifyingAssignments extends AssignmentSubproblem("unsat", satisfying = false)

  object CheckValidity extends Subproblem("valid", 1, 1) {
    var ownFormula: Form = null
    var valid: Boolean = false

    override def init(): Unit =
      ownFormula = PropFormulaGenerator.generate(vars = vars, minDepth = 2, maxDepth = 4, minVars = 2)
      valid = PropLogic.isValid(ownFormula)

    def question() = x"Is $ownFormula valid (true under every assignment)? Answer yes or no."
    def solution() = x"${if valid then "yes" else "no"}"

    override def checkSolution(input: String): CheckResult =
      parseYesNo(input) match
        case None      => NotCheckable(solution().toText)
        case Some(ans) =>
          if ans == valid then Correct()
          else Incorrect(s"The correct answer is: ${if valid then "yes" else "no"}.")
  }

  // Not valid, but changing exactly one And/Or/Implies connective makes it
  // valid - regenerate a fresh formula until we find one with at least one
  // such fix (cheap: small trees, few vars, so this is found almost
  // immediately in practice; capped so a bad parameter combo can't hang).
  object MakeValid extends Subproblem("makevalid", 2, 4) {
    var ownFormula: Form = null
    var fixes: List[Form] = Nil

    override def init(): Unit =
      var attempts = 0
      fixes = Nil
      while fixes.isEmpty && attempts < 500 do
        attempts += 1
        ownFormula = PropFormulaGenerator.generate(vars = vars, minDepth = 2, maxDepth = 4, minVars = 2)
        if !PropLogic.isValid(ownFormula) then
          fixes = PropLogic.oneConnectiveSwaps(ownFormula).filter(PropLogic.isValid).distinct

    def question() =
      x"The formula $ownFormula is not valid, but changing exactly one connective" +
        x" (and/or/implies) turns it into a valid formula. Give one such valid formula."

    def solution(): SText = fixes match
      case Nil          => x"(no single-connective fix found - please regenerate)"
      case one :: Nil   => x"$one"
      case many         => SSnippet(List(x"Any one of:", SItemize(many.map(f => SPlainText(f.toText))*)))

    override def checkSolution(input: String): CheckResult =
      try
        val userForm = PropLogic.parseForm(input)
        if !PropLogic.isValid(userForm) then Incorrect("Your formula is not valid.")
        else if !fixes.contains(userForm) then
          Incorrect("Your formula isn't reachable by changing just one connective of the original.")
        else Correct()
      catch case e: Exception => Incorrect(s"Could not parse your formula: ${e.getMessage}")
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
      parseYesNo(input) match
        case None      => NotCheckable(solution().toText)
        case Some(ans) =>
          if ans == equivalent then Correct()
          else Incorrect(s"The correct answer is: ${if equivalent then "yes" else "no"}.")
  }

  GroupConstraint(1, 1, ConvertToCNF)
  GroupConstraint(1, 1, ConvertToDNF)
  GroupConstraint(1, 1, CheckEquivalence)
  GroupConstraint(1, 1, GiveSatisfyingAssignments)
  GroupConstraint(1, 1, GiveFalsifyingAssignments)
  GroupConstraint(1, 1, CheckValidity)
  GroupConstraint(1, 1, MakeValid)
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
  val weights = ConnectiveWeights(and = 30, or = 30, implies = 32, not = 8)

  def make(): PropLogicProblem =
    val formula = PropFormulaGenerator.generate(vars, minDepth, maxDepth, minVars, weights)
    log("chosen formula: " + formula.toText)
    PropLogicProblem(formula)
}
