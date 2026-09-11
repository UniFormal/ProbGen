package info.kwarc.probgen

import SText._
import Expr._

/**   */
case class BasicProbabilityProblem(setting: BasicProbability) extends Problem[BasicProbabilityProblem] {
  def intro() = {
    val varsLower = setting.varNames.map(_.toLowerCase)
    val cellHead = ~ Prob(setting.varNames.zip(varsLower).map({case (rv,v) => rv === v}), Nil)
    val cells = Range(0,setting.numEvents).flatMap(i => Range(0,setting.numVars).map(j => (i,j, ~ DInt(setting.events(i)(j)))))
    SSnippet(List(
      x"Consider the following joint probability distribution of random variables ${setting.varNames}.",
      SCenter(Seq(STabular(cellHead, varsLower.map(v => ~v), setting.eventNames.map(n => ~n), cells.toList)))
    ))
  }
  def eventNamesShort = ~ RangeSet(setting.eventNames.head, setting.eventNames.last)

  object GiveProb extends Subproblem("giveprob", 2, 2) {
    var form: Form = null
    var result: (Int,Expr) = (0,null)
    /** the expected answer as a set of event names, e.g. Set("a","c","f") */
    var expected: Set[String] = Set.empty
    override def init() = {
      while (result._1 == 0 || result._1 > 4) {
        form = Generator.genForm(State(setting.varNames, maxDepth = 2))
        result = setting.prob(form)
      }
      expected = setting.probLabels(form)
    }
    def question() = x"Give the probability of $form in terms of the ${eventNamesShort}."
    def solution() = x"${result._2}"
    // the answer is a sum of event names, so order does not matter and
    // listing only some of them earns partial credit
    override def grader = Some(TermSumGrader(expected, setting.allEventLabels))
  }

  object GiveCondProb extends Subproblem("givecondprob",2,2) {
    var form: Form = null
    var cond: Form = null
    // (p, q, (sum of p terms) / (sum of q terms))
    var result: (Int,Int,Expr) = (0,0,null)
    /** the expected answer as (numerator names, denominator names) */
    var expectedNum: Set[String] = Set.empty
    var expectedDen: Set[String] = Set.empty
    override def init() = {
      // result._1 = possibleEvents, result._2 = trueEvents
      // Ensure 2 <= possibleEvents <= 6 and 1 <= trueEvents <= 4 and trueEvents != possibleEvents
      while (result._1 < 2 || result._1 > 6 || result._2 < 1 || result._2 > 4 || result._2 == result._1) {
        form = Generator.genForm(State(setting.varNames,maxDepth = 2))
        cond = Generator.genForm(State(setting.varNames,maxDepth = 1))
        result = setting.condProb(form,cond)
      }
      val (n, d) = setting.condProbLabels(form, cond)
      expectedNum = n
      expectedDen = d
    }
    def question() = x"Give the probability of $form given $cond in terms of the $eventNamesShort."
    def solution() = x"${result._3}"
    // numerator and denominator are graded separately and averaged, so getting
    // the conditioning set right still earns credit if the numerator is wrong
    override def grader =
      Some(RatioGrader(expectedNum, expectedDen, setting.allEventLabels))
  }

  val _ = GroupConstraint(2,2,GiveProb,GiveCondProb)
}

