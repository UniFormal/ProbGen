package info.kwarc.probgen

import scala.util.Random

case class Frame(allowLiterals: Boolean = true)

/** state that is used by some functions in [[Generator]] */
class State(val minSize: Int, val maxSize: Int, val vars: List[String]) {
  private var stack = List(Frame())
  def frame = stack.head
  var size = 0
  var unusedVars = vars
  var ints : List[Int] = Nil

  def scope[A](code: => A): A = {
    stack = Frame()::stack
    val r = code
    stack = stack.tail
    r
  }
  def current(f: Frame => Frame) = {
    stack = f(stack.head) :: stack.tail
  }
  def usedVar(n: String) = {
    unusedVars = unusedVars.filterNot(_ == n)
  }
  def noMoreLiterals() = {
    current {f => f.copy(allowLiterals = false)}
  }
}

/**
  * This objects collects utility methods for generating random objects.
  * It was originally intended to allow generating random [[Formula]]s and [[Term]]s
  * based on certain criteria such as depth of the syntax tree,
  * but it is not clear how well this will work.
  * Either way the basic functions are good for reuse.
  */ 
object Generator {
  /* basic functions that do not work with expressions */
  /** choose one value from a list */
  def choose[A](options: List[A]): A = {
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
  /** choose an integer from a range */
  def chooseInt(min: Int, max: Int) = {
    val n = Random.nextInt(max+1-min)
    min+n
  }

  /* methods that try to generate random terms and formulas */

  def genTerm(state: State): Term = {
    val leafProb: Double = if (state.size < state.minSize) 0.0
    else if (state.size >= state.maxSize) 1.0
    else (state.size-state.minSize)/(state.maxSize-state.minSize)
    if (chooseBoolean(leafProb)) {
      val makeLitOverVar = if (!state.frame.allowLiterals) 0.0
      else if (state.unusedVars.nonEmpty) 0.1
      else 0.3
      if (chooseBoolean(makeLitOverVar)) {
        genInt(state)
      } else {
        genVar(state)
      }
    } else {
      val op = choose(TOper.all)
      state.size += 1
      val args = genTerms(state,op.minArity.getOrElse(2),op.maxArity.getOrElse(2))
      Apply(op,args)
    }
  }
  def genTerms(state: State, atLeast: Int = 1, atMost: Int = 3): List[Term] = {
    val len = chooseInt(atLeast,atMost)
    state.scope {
      Range(0,len).toList.map {i =>
        val t = genTerm(state)
        t match {
          case _: Lit => state.noMoreLiterals()
          case _: Apply => state.size += 1
          case _ =>
        }
        t
      }
    }
  }

  val normalInts = List(0,1,2)
  def genInt(state: State): Lit = {
    val extraInts = state.ints
    val i = choose(normalInts:::extraInts)
    Lit(i)
  }
  def genVar(state: State): Var = {
    val vars = state.unusedVars
    val candidates = if (vars.isEmpty) state.vars else vars
    val n = choose(candidates)
    Var(n)
  }
  def genForm(state: State): Form = {
    val op = choose(FOper.all)
    state.size += 1
    val args = genTerms(state,2,2)
    Pred(op,args)
  }
}