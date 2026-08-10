package info.kwarc.probgen

import info.kwarc.probgen.Formula.Not

enum Formula():
      case Var(name : String)
      case Not(f : Formula)
      case And(left : Formula, right : Formula)
      case Or(left : Formula, right : Formula)
      case Implies(left :Formula, right :Formula)


trait PropLogic{

  val formula :Formula

  def apply(formula:Formula):Form =
    formula match{
  case Formula.Var(n) => BVar(n)
  case Formula.Not(f) => Conn(Neg,List(apply(f)))
  case Formula.And(l,r) => Conn(And,List(apply(l),apply(r)))
  case Formula.Or(l,r) => Conn(Or,List(apply(l),apply(r)))
  case Formula.Implies(l,r) => Conn(Implies,List(apply(l),apply(r)))
    }

   

}


object PropLogic{
  // we know that 
  def findSatifsfyingassignment(form:Form):Context = 

    Context(List(("x",1)))

  // This converts from and form to cnf
  def convertCNF(form:Form):Option[Form] =
    form match
      case Conn(Implies,args) => {
        // the first condition is that the list is has atleast 2 elements
       args match
        case xy :+ x => {
          if xy.isEmpty then
            convertCNF(x)
          else
            val first = convertCNF(Conn(Neg,List(Conn(Implies,xy)))).get
            val second = convertCNF(x).get
            Some(Conn(Or,List(first,second)))
        }
        case Nil => None 
      }
      case Conn(And,args) => {
        val x = args.map(convertCNF).map(f => f.get)
        Some(Conn(And,x))
        }
      case Conn(Or,args) => {
        val x = args.map(convertCNF).map(f => f.get)
        Some(Conn(Or,x))
      }
      case Conn(Neg,args) => {
        val x = args.map(convertCNF).map(f => f.get)
        Some(Conn(Neg,x))
      }
      case BVar(name) => Some(BVar(name))
      case _ => None
    
}



object Mytest{

  def main(args:Array[String]) :Unit = 
    val x = Conn(Implies,List(BVar("A"),BVar("B"),BVar("C")))
    val y = PropLogic.convertCNF(x).get
    println(y.toText)
}