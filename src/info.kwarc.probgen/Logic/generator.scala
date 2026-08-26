package info.kwarc.probgen

/** Relative frequency ("percentage") of each connective when generating a
  * random formula. Values don't need to add up to 100 - they are normalized
  * against their own total (see [[Generator.chooseWeighted]]), so e.g.
  * ConnectiveWeights(and = 3, or = 1, implies = 0, not = 0) means "And" is
  * picked 3x as often as "Or", and Implies/Not never get picked at all.
  */
case class ConnectiveWeights(
    and: Double = 30,
    or: Double = 30,
    implies: Double = 32,
    not: Double = 8
)

/** State threaded through generation: how deep the formula currently is and
  * which variables have been used so far - mirrors [[State]], the analogous
  * bookkeeping [[Generator.genTerm]] uses when generating arithmetic terms.
  */
private case class PropFormState(
    vars: List[String],
    minDepth: Int,
    maxDepth: Int,
    minVars: Int,
    weights: ConnectiveWeights
) {
  var usedVars: List[String] = Nil
  var depth: Int = 0
  def unusedVars: List[String] = vars.diff(usedVars)
  def numUsedVars: Int = usedVars.length
}

/** Randomly generates propositional formulas ([[Form]]s built from
  * And/Or/Implies/Neg over BVar leaves), similar in spirit to
  * [[SearchProblemGenerator]] and to [[Generator.genTerm]]: at every node we
  * either stop and emit a variable (a leaf) or expand into a connective,
  * with which connective picked according to caller-supplied percentages.
  *
  * Be careful not to give contradictory criteria (e.g. minVars=4 with
  * maxDepth=1, which can never produce more than one variable) - same caveat
  * as [[SearchProblemGenerator]].
  */
object PropFormulaGenerator {

  // default params for make(), analogous to SearchProblemGenerator's
  // minStates/maxStates/... constants
  val defaultVars: List[String] = List("p", "q", "r", "s")
  val defaultMinDepth = 1
  val defaultMaxDepth = 3
  val defaultMinVars = 2
  val defaultWeights: ConnectiveWeights = ConnectiveWeights()

  def log(s: String) = println("% " + s)

  /** Generate one random formula using the default parameters above. */
  def make(): Form =
    generate(defaultVars, defaultMinDepth, defaultMaxDepth, defaultMinVars, defaultWeights)

  /** Generate a random propositional formula.
    *
    * @param vars     pool of variable names to draw leaves from
    * @param minDepth minimum nesting depth before a leaf is allowed to be chosen
    * @param maxDepth maximum nesting depth - a leaf is forced once this is reached
    * @param minVars  minimum number of distinct variables that must appear
    *                 (capped at vars.length)
    * @param weights  relative frequency ("percentage") of each connective;
    *                 e.g. ConnectiveWeights(implies = 60, and = 40, or = 0, not = 0)
    *                 generates mostly implications with some conjunctions and
    *                 no disjunctions/negations at all.
    */
  def generate(
      vars: List[String] = defaultVars,
      minDepth: Int = defaultMinDepth,
      maxDepth: Int = defaultMaxDepth,
      minVars: Int = defaultMinVars,
      weights: ConnectiveWeights = defaultWeights
  ): Form = {
    require(vars.nonEmpty, "need at least one variable to generate a formula")
    val state = PropFormState(vars, minDepth, maxDepth, minVars min vars.length, weights)
    genForm(state)
  }

  private def genForm(state: PropFormState): Form = {
    val atMaxDepth = state.depth >= state.maxDepth
    val leafProb: Double =
      if (state.depth < state.minDepth) 0.0
      else if (atMaxDepth) 1.0
      else (state.depth - state.minDepth).toDouble / (state.maxDepth - state.minDepth)
    // don't stop early if we still owe the formula some distinct variables -
    // but this must never override the hard depth cap above, or a formula
    // that hasn't reached minVars yet would recurse forever once maxDepth is
    // reached (genConnective alone can never add a variable, only a leaf can)
    val mustKeepGoing = !atMaxDepth && state.numUsedVars < state.minVars && state.unusedVars.nonEmpty
    if (!mustKeepGoing && Generator.chooseBoolean(leafProb)) genLeaf(state)
    else genConnective(state)
  }

  private def genLeaf(state: PropFormState): Form = {
    val candidates = if (state.unusedVars.nonEmpty) state.unusedVars else state.vars
    val n = Generator.choose(candidates)
    state.usedVars ::= n
    BVar(n)
  }

  private def genConnective(state: PropFormState): Form = {
    val w = state.weights
    val op = Generator.chooseWeighted(
      List(And -> w.and, Or -> w.or, Implies -> w.implies, Neg -> w.not)
    )
    state.depth += 1
    op match {
      case Neg => Conn(Neg, List(genForm(state)))
      case _   => Conn(op, List(genForm(state), genForm(state)))
    }
  }
}
