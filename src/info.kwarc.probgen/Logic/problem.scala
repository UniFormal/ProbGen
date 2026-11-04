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

  private def vars = formula.fvsD.sorted

  def intro(): SText =
    x"Consider the propositional formula $formula over the variables ${vars}." +
      x" Formulas are written using 'and', 'or', 'neg', '->' (or 'implies') and parentheses, e.g. 'neg p or (q and r)'."

  // Grade by parsing the student's answer and checking it is both
  // structurally in the right normal form and equivalent to the original -
  // any valid CNF/DNF is accepted, not just the one particular shape toCNF/
  // toDNF happens to produce.
  private def normalFormGrader(isRightForm: Form => Boolean, formName: String): Grader =
    input => Grader.onParsed(ExprParser.parseForm(input)) { userForm =>
      if !isRightForm(userForm) then
        Incorrect(s"That is not in $formName - check the shape of your formula.")
      else if !PropLogic.isEquivalent(userForm, formula) then
        Incorrect(s"That $formName is not equivalent to the original formula.")
      else Correct()
    }

  object ConvertToCNF extends Subproblem("cnf", 2, 3) {
    override def answerHint = "e.g. (p or not q) and r"
    def question() = x"Convert $formula to Conjunctive Normal Form (CNF)."
    def solution() = x"${PropLogic.toCNF(formula)}"
    override def grader = Some(normalFormGrader(PropLogic.isCNF, "CNF"))
  }

  object ConvertToDNF extends Subproblem("dnf", 2, 3) {
    override def answerHint = "e.g. (p and not q) or r"
    def question() = x"Convert $formula to Disjunctive Normal Form (DNF)."
    def solution() = x"${PropLogic.toDNF(formula)}"
    override def grader = Some(normalFormGrader(PropLogic.isDNF, "DNF"))
  }

  // All 2^|fvars| assignments of a formula over fvars, each a List[(var,value)]
  // aligned to fvars.
  private def allAssignments(fvars: Seq[String]): Seq[Context] = PropLogic.assignments(fvars)

  private def holds(f: Form, assignment: Context): Boolean = Evaluator(f)(using assignment)

  // Shared shape for "list all satisfying/falsifying assignments of ..." -
  // each instance generates its own fresh formula (distinct from `formula`
  // and from each other), so seeing one doesn't spoil the others.
  abstract class AssignmentSubproblem(id: String, satisfying: Boolean) extends Subproblem(id, 2, 4) {
    override def answerHint = "e.g. p=true, q=false; p=false, q=true (or: none)"
    private val kind = if satisfying then "satisfying" else "falsifying"
    var ownFormula: Form = null
    private def ownVars = ownFormula.fvsD.sorted
    override def init() = {
      ownFormula = PropFormulaGenerator.generate(vars = vars, minDepth = 2, maxDepth = 4, minVars = 2)
    }

    private def matching: Seq[Context] = {
      allAssignments(ownVars).filter(a => holds(ownFormula, a) == satisfying)
    }

    def question() =
      x"List all $kind assignments of $ownFormula - every assignment that makes it" +
        x" ${if satisfying then "true" else "false"}." +
        x" Write each as e.g. '${Context(ownVars.toList.map(v => (v, BLit(true)))).toString}', separate" +
        x" several with ';', and write 'none' if there are none."

    def solution(): SText = {
      val ms = matching
      if ms.isEmpty then x"There are no $kind assignments."
      else SSnippet(List(SItemize(ms.map(a => SPlainText(a.toString)) *)))
    }
    override def grader = Some(ContextSetGrader(matching, ownVars.toSet))
  }

  object GiveSatisfyingAssignments extends AssignmentSubproblem("sat", satisfying = true)
  object GiveFalsifyingAssignments extends AssignmentSubproblem("unsat", satisfying = false)

  object CheckValidity extends Subproblem("valid", 1, 1) {
    override def answerHint = "yes or no"
    var ownFormula: Form = null
    var valid: Boolean = false

    override def init(): Unit =
      ownFormula = PropFormulaGenerator.generate(vars = vars, minDepth = 2, maxDepth = 4, minVars = 2)
      valid = PropLogic.isValid(ownFormula)

    def question() = x"Is $ownFormula valid (true under every assignment)? Answer yes or no."
    def solution() = x"${if valid then "yes" else "no"}"

    override def grader = Some(YesNoGrader(valid,
      "Not quite: a formula is valid if it is true under every assignment."))
  }

  // Not valid, but changing exactly one And/Or/Implies connective makes it
  // valid - regenerate a fresh formula until we find one with at least one
  // such fix (cheap: small trees, few vars, so this is found almost
  // immediately in practice; capped so a bad parameter combo can't hang).
  object MakeValid extends Subproblem("makevalid", 2, 4) {
    override def answerHint = "a formula, e.g. p -> (q or p)"
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
      case many         => SSnippet(List(x"Any one of:", SItemize(many.map(f => ~f)*)))

    // flattened, since the page shows (p or q) or r as p ∨ q ∨ r
    override def grader = Some(input => Grader.onParsed(ExprParser.parseForm(input)) { userForm =>
      if !PropLogic.isValid(userForm) then Incorrect("Your formula is not valid.")
      else if !fixes.map(PropLogic.flatten).contains(PropLogic.flatten(userForm)) then
        Incorrect("Your formula is valid, but it is not the original with exactly one connective changed.")
      else Correct()
    })
  }

  object CheckEquivalence extends Subproblem("equiv", 1, 1) {
    override def answerHint = "yes or no"
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

    override def grader = Some(YesNoGrader(equivalent,
      "Not quite: two formulas are equivalent if they have the same value under every assignment."))
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
    log("chosen formula: " + formula)
    PropLogicProblem(formula)
}
