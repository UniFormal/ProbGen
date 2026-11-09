package info.kwarc.probgen

import SText._
import Expr._

/** practice of the notation used in answers (sets, pairs, functions, formulas)
  *
  * Every answer is read by [[ExprParser]], so these questions also document its syntax
  * and test it: their answers cover sets, pairs, numbers and truth values.
  */
case class NotationProblem() extends Problem[NotationProblem] {

  private def set(xs: Iterable[Int]): Expr = FinSet(xs.toList.sorted.map(DInt(_))*)
  private def randomSet(): Set[Int] = Generator.chooseSome(Range(0, 10).toList, 3, 4, false).toSet
  private def value(e: Expr): Int = Evaluator(e)(using Context()).asInt

  def intro() =
    x"These questions practise the notation for answers. Every answer field shows an example of what to type." +
      x" Pairs such as ${Tuple(DInt(1), DInt(2))} may be typed as (1, 2), and symbols as words: 'and', 'or', 'not', 'in'."

  object setOperation extends Subproblem("setop", 2, 2) {
    var a: Set[Int] = Set.empty
    var b: Set[Int] = Set.empty
    var op: TOper = Intersection
    override def init() = {
      a = randomSet(); b = randomSet()
      op = Generator.choose(List(Intersection, Union, SetDiff))
    }
    private def result = op match {
      case Intersection => a intersect b
      case Union        => a union b
      case _            => a diff b
    }
    override def answerHint = "a set, e.g. {1, 4} (or {} if it is empty)"
    override def grader = Some(ExprSetGrader(result.toList.map(DInt(_)), "element"))
    def question() = x"Give ${op(set(a), set(b))}."
    def solution() = x"${set(result)}"
  }

  object functionAsPairs extends Subproblem("function", 2, 3) {
    var n = 3
    var body: Expr = Var("x")
    override def init() = {
      n = Generator.chooseInt(2, 4)
      val k = Generator.chooseInt(1, 3)
      body = Generator.choose(List(Plus(Var("x"), DInt(k)), Times(DInt(k + 1), Var("x")), Exp(Var("x"), DInt(2))))
    }
    private def pairs = Range(0, n + 1).toList.map { x =>
      Tuple(DInt(x), DInt(Evaluator(body)(using Context("x" -> x)).asInt))
    }
    override def answerHint = "a set of pairs, e.g. {(0, 1), (1, 2)}"
    override def grader = Some(ExprSetGrader(pairs, "pair"))
    def question() =
      x"Give the function ${MapsTo(Var("x"), body)} for the domain ${RangeSet(DInt(0), DInt(n))} as a set of pairs."
    def solution() = x"${FinSet(pairs*)}"
  }

  object cartesianProduct extends Subproblem("cart", 2, 2) {
    var left: List[String] = Nil
    var right: List[Int] = Nil
    override def init() = {
      left = Generator.chooseSome(List("a", "b", "c"), 2, 2, false).sorted
      right = Generator.chooseSome(List(1, 2, 3), 2, 3, false).sorted
    }
    private def pairs = for (l <- left; r <- right) yield Tuple(Var(l), DInt(r))
    override def answerHint = "a set of pairs, e.g. {(a, 1), (b, 1)}"
    override def grader = Some(ExprSetGrader(pairs, "pair"))
    def question() = x"Give ${Cart(FinSet(left.map(Var(_))*), set(right))}."
    def solution() = x"${FinSet(pairs*)}"
  }

  object cardinality extends Subproblem("card", 1, 1) {
    var a: Set[Int] = Set.empty
    var b: Set[Int] = Set.empty
    var op: TOper = Union
    override def init() = {
      a = randomSet(); b = randomSet()
      op = Generator.choose(List(Union, Intersection, Cart))
    }
    private def size = op match {
      case Union        => (a union b).size
      case Intersection => (a intersect b).size
      case _            => a.size * b.size
    }
    override def answerHint = "a number, e.g. 5"
    override def grader = Some(NumberGrader(size))
    def question() = x"Give ${Card(op(set(a), set(b)))}, the number of elements of ${op(set(a), set(b))}."
    def solution() = x"${DInt(size)}"
  }

  object truthValue extends Subproblem("truth", 1, 1) {
    var formula: Expr = DBool(true)
    var truth = true
    /** a membership or a comparison, together with its truth value */
    private def atom(): (Expr, Boolean) =
      if (Generator.chooseBoolean(0.5)) {
        val s = randomSet(); val n = Generator.chooseInt(0, 9)
        (InSet(DInt(n), set(s)), s.contains(n))
      } else {
        val x = Generator.chooseInt(0, 9); val y = Generator.chooseInt(0, 9)
        Generator.choose(List(
          (Less(DInt(x), DInt(y)), x < y), (LessEq(DInt(x), DInt(y)), x <= y), (Equals(DInt(x), DInt(y)), x == y)))
      }
    override def init() = {
      val (f1, t1) = atom()
      val (f2, t2) = atom()
      val (f, t) = Generator.choose(List(
        (And(f1, f2), t1 && t2), (Or(f1, f2), t1 || t2), (And(Neg(f1), f2), !t1 && t2)))
      formula = f; truth = t
    }
    override def answerHint = "yes or no"
    override def grader = Some(YesNoGrader(truth, "Not quite: evaluate each part first, then combine them."))
    def question() = x"Is ${formula} true? Answer yes or no."
    def solution() = x"${if (truth) "yes" else "no"}"
  }

  object arithmetic extends Subproblem("arith", 1, 1) {
    var term: Expr = DInt(0)
    override def init() = {
      val List(a, b, c) = List.fill(3)(DInt(Generator.chooseInt(1, 9)))
      term = Generator.choose(List(
        Mod(Times(Plus(a, b), c), DInt(Generator.chooseInt(3, 9))),
        Minus(Times(a, b), c),
        Plus(a, Times(b, c))))
    }
    override def answerHint = "a number, e.g. 12"
    // the value itself, not the expression typed back in
    override def grader = Some(input => Grader.onParsed(ExprParser.parseExpr(input)) {
      case l: Lit if ExprCompare.numericValue(l).isDefined =>
        if (ExprCompare.numericValue(l).get == value(term)) Correct() else Incorrect("Not quite.")
      case _ => MalformedInput("Give the value as a single number.")
    })
    def question() = x"Give the value of ${term}."
    def solution() = x"${DInt(value(term))}"
  }

  GroupConstraint(1, 1, setOperation)
  GroupConstraint(1, 1, functionAsPairs)
  GroupConstraint(1, 1, cartesianProduct)
  GroupConstraint(1, 1, cardinality)
  GroupConstraint(1, 1, truthValue)
  GroupConstraint(1, 1, arithmetic)
}
