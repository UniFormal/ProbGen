package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B1: Context as an expression, and evaluating with real numbers */
class ContextAndEvaluatorTest extends AnyFunSuite {

  test("a Context is an expression") {
    val c: Expr = Context()("a" -> 1)
    assert(c.isInstanceOf[Context])
  }

  test("equal Contexts: the order of the bindings does not matter") {
    val pq = Context()("p" -> true)("q" -> false)
    val qp = Context()("q" -> false)("p" -> true)
    assert(pq == qp)
    assert(pq.hashCode == qp.hashCode)
    assert(Set(pq, qp).size == 1)
    assert(pq != Context()("p" -> true)("q" -> true))
    assert(pq != Context()("p" -> true))
  }

  test("a name bound twice means its newest value") {
    val c = Context()("a" -> 1)("a" -> 2)
    assert(c("a") == DInt(2))
    assert(c == Context()("a" -> 2))
  }

  test("display: sorted by name, in plain text, sTeX and MathML") {
    val c = Context()("b" -> 2)("a" -> 1)
    assert(c.toString == "a=1, b=2")
    assert(c.toSTeX == "\\eq{a,1},\\;\\eq{b,2}")
    assert(c.toHTML.contains("<mi>a</mi><mo>=</mo><mn>1</mn>"))
  }

  test("Evaluator.real computes with decimals and divides exactly") {
    assert(Evaluator.real(Divide(DInt(3), DInt(5))) == 0.6)
    assert(Evaluator.real(Times(DDouble(0.8), DDouble(0.8))) == 0.8 * 0.8)
    assert(Evaluator.real(Plus(DInt(1), DDouble(0.5), DInt(2))) == 3.5)
    assert(Evaluator.real(Minus(DInt(1), DDouble(0.25))) == 0.75)
    assert(Evaluator.real(Exp(DInt(2), DInt(10))) == 1024.0)
    assert(Evaluator.real(Var("x"), Context()("x" -> 4)) == 4.0)
  }

  test("Evaluator.real reports what it cannot compute") {
    assertThrows[EvalError](Evaluator.real(Divide(DInt(1), DInt(0))))
    assertThrows[EvalError](Evaluator.real(DString("a")))
    assertThrows[EvalError](Evaluator.real(Var("x")))
  }

  test("Symbols: from a typed character back to its LaTeX command") {
    assert(Symbols.command("γ") == Some("gamma"))
    assert(Symbols.command("→") == Some("rightarrow"))
    assert(Symbols.command("x") == None)
  }
}
