package info.kwarc.probgen

import SText._

object BasicProbability {
  def enumerate(sig: List[Int]): List[List[Int]] = sig match {
    case Nil => List(Nil)
    case hd::tl => Range(0,hd).toList.flatMap(h => enumerate(tl).map(t => h::t))
  }
}

class BasicProbability(val domainSizes: List[Int]) {
  // e: Event is valid only if e.length == numVars
  type Event = List[Int]
  val numVars = domainSizes.length
  // ..., X, Y, Z
  val varNames = Range(0,numVars).toList.map(i => NameLit.applyUpper(26-numVars+i).value.asInstanceOf[String])
  // [0,...,0,0], [0,...,0,1], ..., [domainSize(0)-1, ..., domainSize(-1)-1]
  val events: List[Event] = BasicProbability.enumerate(domainSizes)
  val numEvents = events.length
  // a, b, c, ...
  val eventNames = Range(0,numEvents).toList.map(i => NameLit(i))
  def eventName(e: Event) = {
    val j = e.zipWithIndex.map {case (v,i) => v*domainSizes.drop(i+1).product}.sum
    eventNames(j)
  }
  def eventSum(es: List[Event]) = Plus(es.map(e => DString(eventName(e)))*)
  def valid = numVars + numEvents <= 26
  def eval(form: Form, ev: Event) = {
    Evaluator(form)(Context(varNames.zip(ev)))
  }
  // f must be an expression using the variables
  // P(f) in terms of event names
  // (p, sum of p terms)
  def prob(f: Form) = {
    val trueEvents = events.filter(e => eval(f,e))
    val l = trueEvents.length
    (l, if (l == 0) DInt(0) else if (l == numEvents) DInt(1) else eventSum(trueEvents))
  }
  // P(f | cond) in terms of event names
  // (p, q, (sum of p terms) / (sum of q terms))
  def condProb(f: Form, cond: Form) = {
    val possibleEvents = events.filter(e => eval(cond,e))
    val trueEvents = possibleEvents.filter(e => eval(f,e))
    val r = if (possibleEvents.isEmpty) DInt(0)
      else if (possibleEvents == trueEvents) DInt(1)
      else Divide(eventSum(trueEvents), eventSum(possibleEvents))
    (possibleEvents.length, trueEvents.length, r)
  }
}