package info.kwarc.probgen

object LogicProblemGenerator extends ProblemGenerator[PropLogicProblem]{
    def make() : PropLogicProblem = {
        val depth = Generator.chooseInt(2,10)
        val formula = genFormula(List("p","q","r"),depth)
        PropLogicProblem(formula,depth)
    }


    def genFormula(vals : List[String],depth : Int) : Formula = 
       if depth == 0 then 
        Formula.Var(Generator.choose(vals))
       else
        val x = Generator.choose(List(1,2,3))
        x match
            case 1 => Formula.Var(Generator.choose(vals))
            case 2 => Formula.Not(genFormula(vals,depth-1))
            case _ => {
                val y = Generator.choose(List(And,Or,Implies))
                val f1 = genFormula(vals,depth-1)
                val f2 = genFormula(vals,depth-1)
                y match
                    case And => Formula.And(f1,f2)
                    case Or => Formula.Or(f1,f2)
                    case _ => Formula.Implies(f1,f2)
            }
        

}