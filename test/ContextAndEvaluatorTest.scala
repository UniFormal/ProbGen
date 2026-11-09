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

  test("numeric values (what grading uses) compute with decimals and divide exactly") {
    import ExprCompare.numericValue
    assert(numericValue(Divide(DInt(3), DInt(5))) == Some(0.6))
    assert(numericValue(Divide(DInt(16), DInt(25))) == Some(0.64))
    assert(numericValue(Times(DDouble(0.8), DDouble(0.8))) == Some(0.8 * 0.8))
    assert(numericValue(Plus(DInt(1), DDouble(0.5), DInt(2))) == Some(3.5))
    assert(numericValue(Minus(DInt(1), DDouble(0.25))) == Some(0.75))
    assert(numericValue(Exp(DInt(2), DInt(10))) == Some(1024.0))
  }

  test("numeric values: anything that is not a number gives None, never an exception") {
    import ExprCompare.numericValue
    assert(numericValue(Divide(DInt(1), DInt(0))) == None)
    assert(numericValue(DString("a")) == None)
    assert(numericValue(Var("x")) == None)
    assert(numericValue(FinSet(DInt(3), DInt(5))) == None)
    assert(numericValue(Tuple(DInt(0), DInt(1))) == None)
    assert(numericValue(DBool(true)) == None)
  }

  test("integer arithmetic stays integer in the Evaluator (Search and CSP need it)") {
    assert(Evaluator(Divide(DInt(7), DInt(2)))(using Context()) == DInt(3))
    assert(Evaluator(Mod(Minus(DInt(1), DInt(3)), DInt(5)))(using Context()) == DInt(3))
  }

  test("decimal literals can be displayed") {
    assert(DDouble(0.5).toHTML.contains("0.5"))
    assert(DDouble(0.5).toSTeX == "0.5")
  }

  test("Symbols: from a typed character back to its LaTeX command") {
    assert(Symbols.command("γ") == Some("gamma"))
    assert(Symbols.command("→") == Some("rightarrow"))
    assert(Symbols.command("x") == None)
  }
}
