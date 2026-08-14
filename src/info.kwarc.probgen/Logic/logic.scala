package info.kwarc.probgen

import info.kwarc.probgen.Formula.Not

enum Formula():
  case Var(name: String)
  case Not(f: Formula)
  case And(left: Formula, right: Formula)
  case Or(left: Formula, right: Formula)
  case Implies(left: Formula, right: Formula)

trait PropLogic {

  val formula: Formula

  def apply(formula: Formula): Form =
    formula match {
      case Formula.Var(n)        => BVar(n)
      case Formula.Not(f)        => Conn(Neg, List(apply(f)))
      case Formula.And(l, r)     => Conn(And, List(apply(l), apply(r)))
      case Formula.Or(l, r)      => Conn(Or, List(apply(l), apply(r)))
      case Formula.Implies(l, r) => Conn(Implies, List(apply(l), apply(r)))
    }

}

object PropLogic {
  // we know that
  def findassignment(
      form: Form,
      satisfying: Boolean
  ): Option[Context] =
    val collected = collectVars(form)
    val size = collected.size
    val assignments = efficientAssignment(size)
    val ans = assignments.find(xs => {
      val sq = xs.toList
      val zipped = collected.toList.zip(sq)
      val ctx = Context(zipped)
      val eval = Evaluator(form)(using ctx)
      if satisfying then eval
      else !eval
    })
    ans.map(f => {
      val x = collected.toList.zip(f.toList)
      Context(x)
    })

  def assignments(n: Int): LazyList[List[Boolean]] =
    if n == 0 then LazyList(List.empty)
    else
      for
        rest <- assignments(n - 1)
        bit <- LazyList(false, true)
      yield bit :: rest

  def efficientAssignment(n: Int): LazyList[Array[Boolean]] =
    LazyList.from(0).take(1 << n).map { mask =>
      Array.tabulate(n)(i => (mask & (1 << i)) != 0)
    }

  // This converts from and form to cnf
  def convertCNF(form: Form): Option[Form] =
    form match
      case Conn(Implies, args) => {
        // the first condition is that the list is has atleast 2 elements
        args match
          case xy :+ x => {
            if xy.isEmpty then convertCNF(x)
            else
              val first = convertCNF(Conn(Neg, List(Conn(Implies, xy)))).get
              val second = convertCNF(x).get
              Some(Conn(Or, List(first, second)))
          }
          case Nil => None
      }
      case Conn(And, args) => {
        val x = args.map(convertCNF).map(f => f.get)
        Some(Conn(And, x))
      }
      case Conn(Or, args) => {
        val x = args.map(convertCNF).map(f => f.get)
        Some(Conn(Or, x))
      }
      case Conn(Neg, args) => {
        val x = args.map(convertCNF).map(f => f.get)
        Some(Conn(Neg, x))
      }
      case BVar(name) => Some(BVar(name))
      case _          => None

  def toNNF(form: Form): Form =
    form match
      case Conn(Neg, args) => {
        Conn(Neg, args)
      }
      case a => a

  def collectVars(form: Form): Set[String] =
    var collector: Set[String] = Set()
    form match
      case BVar(n) => {
        collector = collector + n
      }
      case Conn(_, sq) => {
        val x = sq.flatMap(collectVars)
        collector = collector ++ x
      }
      case _ => {}
    collector

}

object Mytest {

  def main(args: Array[String]) =
    val x = Conn(Implies, List(BVar("A"), BVar("B"), BVar("C")))
    val m = Conn(
      Or,
      List(
        Conn(And, List(BVar("A"), BVar("B"))),
        Conn(And, List(BVar("C"), BVar("D")))
      )
    )
    val k = Conn(Or, List(BVar("A"), Conn(Neg, List(BVar("A")))))
    val ans = PropLogic.findassignment(k, false)
    if ans.isDefined then
      val x = ans.get
      println(x)
    else println("can't find any assignments")

}
