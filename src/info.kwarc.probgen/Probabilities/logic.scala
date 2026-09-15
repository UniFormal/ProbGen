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
  def eventSum(es: List[Event]) = Plus(es.map(e => eventName(e))*)
  def valid = numVars + numEvents <= 26

  // -- plain-string views of the event names, for answer checking -------------
  /** the bare name of an event, e.g. "a" — the form a student types */
  def eventLabel(e: Event): String = eventName(e).value.toString
  /** every event name in this setting, used to spot typos in an answer */
  def allEventLabels: Set[String] = events.map(eventLabel).toSet
  /** the events making up P(f), as names */
  def probLabels(f: Form): Set[String] = events.filter(e => eval(f, e)).map(eventLabel).toSet
  /** the events of P(f | cond), as (numerator names, denominator names) */
  def condProbLabels(f: Form, cond: Form): (Set[String], Set[String]) = {
    val possible = events.filter(e => eval(cond, e))
    val trueEvents = possible.filter(e => eval(f, e))
    (trueEvents.map(eventLabel).toSet, possible.map(eventLabel).toSet)
  }

  def eval(form: Form, ev: Event) = {
    Evaluator(form)(using Context(varNames.zip(ev.map(DInt(_)))))
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