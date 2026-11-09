package info.kwarc.probgen

import SText.*

/**
  * preconditions: FOL.wff(sig); FOL.wff(sig,form); FOL.wff(sig,term); FOL.wff(sig,mod)
  */
case class FOLProblem(sig: FOLSignature, mod: FOLFiniteModel, form: Expr, term: Expr) extends Problem[FOLProblem] {
  def intro() = x"Consider the following signature of first-order logic: ..."

  object GiveFormula extends Subproblem("giveform",2,3) {
    def question() = x"Give a well-formed formula that uses all symbols of the signature"
    def solution() = x"E.g., ..."
    override def checkSolution(s: String) = {
      val t = ??? /// parse s
      try {FOL.wft(sig,Nil,t)}
      catch {case FOL.Illformed(msg) => Incorrect(msg)}
      // check if all symbol occur PartiallyCorrect(0.5, "not all symbols occur in the formula")
      Correct()
    }
  }

  object GiveModel extends Subproblem("givemod",2,3) {
    def question() = x"Give a model for that signature in which the formula ... is true" // this.form
    def solution() = x"E.g., ..."
    override def checkSolution(s: String) = {
      val mod = ??? /// parse s
      try {FOL.wfm(sig,mod)}
      catch {case FOL.Illformed(msg) => Incorrect(msg)}
      val b = FOL.interpret(mod, Context(), form).asBool
      if (b) Correct() else PartiallyCorrect(0.5, "the formula is not true")
    }
  }

  object InterpretTerm extends Subproblem("interpret",2,3) {
    val termI = FOL.interpret(mod, Context(), term)
    def question() = x"Consider the following model: ... Give the interpretation of the term ..." // render this.mod, this.term
    def solution() = x"..." // termI
    override def checkSolution(s: String) = {
      val t = DInt(s.toInt)
      if (t == termI) Correct() else Incorrect("not correct")
    }
  }
}
