package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B6 (Probabilities): on random problems, model answers typed in are graded correct,
  * and typical wrong answers are not (the graders themselves were switched in B3)
  */
class ProbabilityGradingTest extends AnyFunSuite {

  private val runs = 40
  private def problems = List.fill(runs)(BasicProbabilityProblemGenerator.make())
  private def sum(names: Iterable[String]) = names.toList.sorted.mkString(" + ")

  test("P(formula) as a sum of event names") {
    for (p <- problems) {
      val q = p.GiveProb
      q.init()
      assert(q.check(sum(q.expected)) == Correct(), sum(q.expected))
      assert(q.check(sum(q.expected.toList.reverse)) == Correct())
      val other = p.setting.allEventLabels.diff(q.expected).head
      assert(q.check(sum(q.expected + other)).isInstanceOf[PartiallyCorrect])
      assert(q.check("a + zz").isInstanceOf[MalformedInput]) // not an event of this problem
    }
  }

  test("P(formula | condition) as a quotient of sums") {
    for (p <- problems) {
      val q = p.GiveCondProb
      q.init()
      val answer = s"(${sum(q.expectedNum)}) / (${sum(q.expectedDen)})"
      assert(q.check(answer) == Correct(), answer)
      // numerator and denominator swapped: partly right at most
      assert(q.check(s"(${sum(q.expectedDen)}) / (${sum(q.expectedNum)})") != Correct())
      assert(q.check(s"${sum(q.expectedNum)} / ${sum(q.expectedDen)}").isInstanceOf[MalformedInput] ||
        q.expectedNum.size == 1 && q.expectedDen.size == 1) // without brackets only a single name each is a quotient
    }
  }
}
