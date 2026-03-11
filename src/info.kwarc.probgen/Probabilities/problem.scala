package info.kwarc.probgen

import SText._

/**   */
case class BasicProbabilityProblem(setting: BasicProbability) extends Problem[BasicProbabilityProblem] {
  def intro() = {
    val varsLower = setting.varNames.map(_.toLowerCase)
    val cellHead = x"§\uProb{${setting.varNames.zip(varsLower).map({case (rv,v) => rv + "=" + v}).mkString(", ")}}§"
    val cells = Range(0,setting.numEvents).flatMap(i => Range(0,setting.numVars).map(j => (i,j,x"§${setting.events(i)(j)}§")))
    SSnippet(List(
      x"Consider the following joint probability distribution of random variables ${setting.varNames}.",
      SCenter(Seq(STabular(cellHead, varsLower.map(v => x"§$v§"), setting.eventNames.map(n => x"§$n§"), cells.toList)))
    ))
  }
  def eventNamesShort = x"§${setting.eventNames.head}§, \ldots, §${setting.eventNames.last}§"

  object GiveProb extends Subproblem("giveprob", 2, 2) {
    var form: Form = null
    var result: (Int,Expr) = (0,null)
    override def init() = {
      while (result._1 == 0 || result._1 > 4) {
        form = Generator.genForm(State(setting.varNames, maxDepth = 2))
        result = setting.prob(form)
      }
    }
    def question() = x"Give the probability of $form in terms of the ${eventNamesShort}."
    def solution() = x"${result._2}"
  }

  object GiveCondProb extends Subproblem("givecondprob",2,2) {
    var form: Form = null
    var cond: Form = null
    // (p, q, (sum of p terms) / (sum of q terms))
    var result: (Int,Int,Expr) = (0,0,null)
    override def init() = {
      while (result._1 == 0 || result._1 > 4 || result._2 < 2 || result._2 > 6 || result._1 == result._2) {
        form = Generator.genForm(State(setting.varNames,maxDepth = 2))
        cond = Generator.genForm(State(setting.varNames,maxDepth = 1))
        result = setting.condProb(form,cond)
      }
    }
    def question() = x"Give the probability of $form given $cond in terms of the $eventNamesShort."
    def solution() = x"${result._3}"
  }

  val _ = GroupConstraint(2,2,GiveProb,GiveCondProb)
}

