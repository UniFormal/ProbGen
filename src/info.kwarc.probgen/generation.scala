package info.kwarc.probgen

import scala.util.Random

case class Frame(allowLiterals: Boolean = true)

/** state that is used by some functions in [[Generator]] */
case class State(vars: List[String], extraInts: List[Int] = Nil,
                 minDepth: Int = 1, maxDepth: Int = 1,
                 minVars: Int = 1, maxVars: Int = 2,
                 minLeaves: Int = 2, maxLeaves: Int = 4) {
  var usedVars: List[String] = Nil
  def unusedVars = vars.diff(usedVars)
  def numUsedVars = usedVars.length
  var depth = 0
  var leaves = 0
  var allowLiteral = true
}

/**
  * This objects collects utility methods for generating random objects.
  * It was originally intended to allow generating random [[Form]]s and [[Term]]s
  * based on certain criteria such as depth of the syntax tree,
  * but it is not clear how well this will work.
  * Either way the basic functions are good for reuse.
  */ 
object Generator {
  /* basic functions that do not work with expressions */
  /** choose one value from a list */
  def choose[A](options: Seq[A]): A = {
    val n = Random.nextInt(options.length)
    options(n)
  }
  /** choose some values from a list */
  def chooseSome[A](options: List[A], atLeast: Int, atMost: Int, allowRep: Boolean) = {
    val num = choose(Range(atLeast,atMost+1).toList)
    var choices: List[A] = Nil
    while (choices.length < num) {
      val c = choose(options)
      if (allowRep || !choices.contains(c)) choices ::= c
    }
    choices
  }
  /** choose a Boolean, true with some probability */
  def chooseBoolean(prob: Double): Boolean = {
    val r = Random.nextFloat()
    r <= prob
  }
  /** choose an integer from a range, min/max included */
  def chooseInt(min: Int, max: Int) = {
    val n = Random.nextInt(max+1-min)
    min+n
  }
  /** choose one value from a list of (value, weight) pairs; the weights are
    * relative frequencies ("percentages") - they don't need to sum to any
    * particular total, they get normalized against their own sum */
  def chooseWeighted[A](options: Seq[(A, Double)]): A = {
    require(options.nonEmpty, "chooseWeighted needs at least one option")
    val total = options.map(_._2).sum
    require(total > 0, "at least one weight must be positive")
    val r = Random.nextDouble() * total
    var acc = 0.0
    var result = options.head._1
    var found = false
    for ((a, w) <- options if !found) {
      acc += w
      if (r < acc) { result = a; found = true }
    }
    result
  }

  /* methods that try to generate random terms and formulas */

  def genTerm(state: State): Term = {
    val leafProb: Double = if (state.depth < state.minDepth) 0.0
    else if (state.depth >= state.maxDepth) 1.0
    else (state.depth-state.minDepth)/(state.maxDepth-state.minDepth)
    if (chooseBoolean(leafProb)) {
      state.leaves += 1
      val uv = state.numUsedVars
      val litProb = if (!state.allowLiteral || uv < state.minVars) 0.0
      else if (uv >= state.maxVars) 0.9
      else 0.1
      if (chooseBoolean(litProb)) {
        genInt(state)
      } else {
        genVar(state)
      }
    } else {
      val op = choose(TOper.all)
      state.depth += 1
      val args = genTerms(state,op.minArity.getOrElse(2),op.maxArity.getOrElse(2))
      Apply(op,args)
    }
  }
  def genTerms(state: State, atLeast: Int = 1, atMost: Int = 3): List[Term] = {
    val len = chooseInt(atLeast,atMost)
    val d = state.depth
    state.allowLiteral = true
    Range(0,len).toList.map {i =>
      state.depth = d
      val t = genTerm(state)
      if (t.isInstanceOf[Lit]) state.allowLiteral = false
      t
    }
  }

  val normalInts = List(0,1,2)
  def genInt(state: State): Lit = {
    val i = choose(normalInts:::state.extraInts)
    DInt(i)
  }
  def genVar(state: State): Var = {
    val vars = state.unusedVars
    val candidates = if (vars.isEmpty) state.vars else vars
    val n = choose(candidates)
    state.usedVars ::= n
    Var(n)
  }
  def genForm(state: State): Form = {
    val op = choose(FOper.all)
    state.depth += 1
    val args = genTerms(state,2,2)
    Pred(op,args)
  }
}