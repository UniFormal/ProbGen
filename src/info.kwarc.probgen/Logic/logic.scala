package info.kwarc.probgen

enum Formula():
      case Var(name : String)
      case Not(f : Formula)
      case And(left : Formula, right : Formula)
      case Or(left : Formula, right : Formula)
      case Implies(left :Formula, right :Formula)


trait PropLogic{

  val identNames :List[String]

  def apply(formula:Formula):Form =
    formula match{
  case Formula.Var(n) => BVar(n)
  case Formula.Not(f) => Conn(Neg,List(apply(f)))
  case Formula.And(l,r) => Conn(And,List(apply(l),apply(r)))
  case Formula.Or(l,r) => Conn(Or,List(apply(l),apply(r)))
  case Formula.Implies(l,r) => Conn(Implies,List(apply(l),apply(r)))
    }
}
