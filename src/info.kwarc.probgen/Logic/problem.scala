package info.kwarc.probgen
import SText.*
case class PropLogicProblem(val formula :Formula,possibleDepth:Int) extends PropLogic with Problem[PropLogicProblem]{
  def intro() :SText = {
    val fs:List[String] = this.collect(formula).toList
    val stex:Form = this(formula)
    val pretty = stex.toSTeX
    SText(
      x"consider the following Propositional Variables ${fs}",
      x"find all valid assignments for the formula ${pretty}"
    ) 
  }
  def collect(form : Formula):Set[String] =
    form match{
      case Formula.Var(x) => Set(x)
      case Formula.And(l,r) => collect(l) ++ collect(r)
      case Formula.Or(l,r) => collect(l) ++ collect(r)
      case Formula.Implies(l,r) => collect(l) ++ collect(r)
      case Formula.Not(x) => collect(x)
    }

  def findAssignment(form:Formula) : Context=
    val vars:List[(String,Boolean)] = this.collect(form).toList.map(st => (st,Generator.chooseBoolean(0.5)))
    Context(vars)
}
