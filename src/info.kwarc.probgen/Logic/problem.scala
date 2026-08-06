package info.kwarc.probgen


case class PropLogicProblem(val identNames : List[String],possibleDepth:Int) extends PropLogic with Problem[PropLogicProblem]{
  def collect(form : Formula):Set[String] =
    form match{
      case Formula.Var(x) => Set(x)
      case Formula.And(l,r) => collect(l) ++ collect(r)
      case Formula.Or(l,r) => collect(l) ++ collect(r)
      case Formula.Implies(l,r) => collect(l) ++ collect(r)
      case Formula.Not(x) => collect(x)
    }

  def findAssignment(form:Formula) : Context=
    val vars:List[(String,Boolean)] = collect(form).toList.toList.map(st => (st,chooseBoolean(0.5)))
    Context(vars)
}
