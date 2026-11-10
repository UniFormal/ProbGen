package info.kwarc.probgen

object VarNames {
  // 0 -> a, 1 -> b, ...
  def apply(i: Int) = (97+i).toChar.toString
  def applyUpper(i: Int) = (65+i).toChar.toString
}

object BasicProbability {
  def enumerate(domainSizes: List[Int]): List[List[Int]] = product(domainSizes.map(s => Range(0,s).toList))

  /** Cartesian product of lists */
  def product[A](doms: List[List[A]]): List[List[A]] = doms match {
    case Nil => List(Nil)
    case hd::tl => hd.flatMap(h => product(tl).map(t => h::t))
  }
}

/** abstract probabilities for a simple random signature
  * @param domainSizes e.g., List(3,2,2) for variables D_X={0,1,2}, D_Y={0,1}, D_Z={0,1}
  */
class BasicProbability(val domainSizes: List[Int]) {
  /** List(1,0,0) represents the event X=1, Y=0, Z=0; must satisfy e.length == numVars */
  type Event = List[Int]
  val numVars = domainSizes.length
  /** ..., X, Y, Z */
  val varNames = Range(0,numVars).toList.map(i => VarNames.applyUpper(26-numVars+i))
  /* all events, i.e., [0,...,0,0], [0,...,0,1], ..., [domainSize.head-1, ..., domainSize.last-1] */
  val events: List[Event] = BasicProbability.enumerate(domainSizes)
  /** product of all domain sizes */
  val numEvents = events.length
  /** a, b, c, ... */
  val eventNames = Range(0,numEvents).toList.map(i => VarNames(i))
  def eventIndex(e: Event) = e.zipWithIndex.map {case (v,i) => v*domainSizes.drop(i+1).product}.sum
  def eventName(e: Event): Lit = DString(eventLabel(e))
  def eventSum(es: List[Event]) = Plus(es.map(e => eventName(e))*)
  /** event are named from start of alphabet, variables from end; sum should be <= 26 */
  def valid = numVars + numEvents <= 26

  // -- plain-string views of the event names, for answer checking -------------
  /** the bare name of an event, e.g. "a" — the form a student types */
  def eventLabel(e: Event) = {
    val j = eventIndex(e)
    eventNames(j)
  }
  /** every event name in this setting, used to spot typos in an answer */
  def allEventLabels: Set[String] = events.map(eventLabel).toSet
  /** the events making up P(f), as names */
  def probLabels(f: Expr): Set[String] = events.filter(e => eval(f, e)).map(eventLabel).toSet
  /** the events of P(f | cond), as (numerator names, denominator names) */
  def condProbLabels(f: Expr, cond: Expr): (Set[String], Set[String]) = {
    val possible = events.filter(e => eval(cond, e))
    val trueEvents = possible.filter(e => eval(f, e))
    (trueEvents.map(eventLabel).toSet, possible.map(eventLabel).toSet)
  }

  /** true if an event satisfies a formula
    * f must be an expression using the variables
    */
  def eval(form: Expr, ev: Event) = {
    Evaluator(form)(using Context(varNames.zip(ev.map(DInt(_))))).asBool
  }

 /** P(f) as an Expr that sums the event names where f is true
   * (p, sum of p terms) where p is a number
   */
  def prob(f: Expr) = {
    val trueEvents = events.filter(e => eval(f,e))
    val l = trueEvents.length
    (l, if (l == 0) DInt(0) else if (l == numEvents) DInt(1) else eventSum(trueEvents))
  }
  /** P(f | cond) as an Expr in the event names
    * (p, q, (sum of p terms) / (sum of q terms))
    */
  def condProb(f: Expr, cond: Expr) = {
    val possibleEvents = events.filter(e => eval(cond,e))
    val trueEvents = possibleEvents.filter(e => eval(f,e))
    val r = if (possibleEvents.isEmpty) DInt(0)
      else if (possibleEvents == trueEvents) DInt(1)
      else Divide(eventSum(trueEvents), eventSum(possibleEvents))
    (possibleEvents.length, trueEvents.length, r)
  }
}

// ****************** finite domain probability in general

/** a random variable consists of name and domain */
case class RandVar(name: String, domain: OrderedDomain)
/** a random signature is a list of random variables */
case class RandSig(vars: List[RandVar]) {
  /** number of variables */
  def numVars = vars.length
  /** list of all events (Cartesian product of domains) */
  val events: List[Context] = {
    val tuples = BasicProbability.product(vars.map(_.domain.values))
    tuples.map {t =>
      val kvs = vars.zip(t).map {case (vr,vl) => (vr.name,Expr(vl))}
      Context(kvs)
    }
  }
  def indexOf(e: Context) = events.indexOf(e)
  def numEvents = events.length
}

/** joint probability distribution for a random signature, given as a list of values in event order */
case class RandDist(sig: RandSig, probs: List[Double]) {
  /** list of events in which a formula holds */
  def trueIn(f: Expr): List[Context] = sig.events.filter(e => Evaluator(f)(using e).asBool)
  /** probability of an event */
  def probOf(e: Context): Double = probs(sig.indexOf(e))
  /** total probability of a set of events */
  def probOf(es: List[Context]): Double = es.map(probOf(_)).sum
  /** conditional probability P(f|cond) */
  def condProbeval(f: Expr,cond: Expr) = {
    val condTrue = trueIn(cond)
    val bothTrue = condTrue.filter(e => Evaluator(f)(using e).asBool)
    probOf(bothTrue) / probOf(condTrue)
  }
}