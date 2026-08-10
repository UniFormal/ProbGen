/*
package info.kwarc.probgen
object Test {
  @main
  def test() =
    val b = List(BVar("p"),BVar("q"),BVar("r"))
    val c = Conn(And,b)
    val ctx = Context(List(("p", true), ("q", true), ("r", true)))
    val y = Evaluator(c)(using ctx)
    val k = LogicProblem()
    val mk = k.createProp()
    println(mk.toSTeX)
    println(y)
    println(c.toSTeX)
}
*/