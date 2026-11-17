package info.kwarc.probgen

object PropLogic {
  def assignments(vs: Seq[String]): List[Context] = vs.toList match {
    case Nil => List(Context())
    case h :: t =>
      val tA = assignments(t)
      tA.flatMap {c => List(c(h -> true),c(h -> false))}
  }

  def findassignment(form: Expr,satisfying: Boolean): Option[Context] = findAllAssignments(form,satisfying).headOption

  def findAllAssignments(form: Expr, satisfying: Boolean): Seq[Context] = {
    val collected = form.fvsD
    assignments(collected).filter {ctx =>
      val eval = Evaluator(form)(using ctx).asBool
      if satisfying then eval else !eval
    }
  }

  // Two formulas are (semantically) equivalent iff they agree on every truth
  // assignment over the union of the variables occurring in either of them.
  def isEquivalent(f1: Expr,f2: Expr): Boolean = {
    val vars = (f1.fvs ++ f2.fvs).distinct
    assignments(vars).forall(ctx =>
      Evaluator(f1)(using ctx) == Evaluator(f2)(using ctx)
    )
  }

  // A formula is valid (a tautology) iff no assignment falsifies it.
  def isValid(form: Expr): Boolean = findAllAssignments(form,false).isEmpty

  // Every formula reachable from `form` by changing exactly one And/Or/
  // Implies node into a *different* one of those three connectives, keeping
  // its children untouched (Neg and the leaves are left alone - there's no
  // same-arity "different connective" to swap a negation or a variable for).
  // Used for "this formula is invalid, but one connective away from valid"
  // exercises.
  def oneConnectiveSwaps(form: Expr): List[Expr] = {
    val alternatives = List(And,Or,Implies)
    form match
      case OperApply(op,args) if alternatives.contains(op) =>
        val here = alternatives.filter(_ != op).map(alt => OperApply(alt,args))
        val deeper = args.indices.toList.flatMap {i =>
          oneConnectiveSwaps(args(i)).map(newChild => OperApply(op,args.updated(i,newChild)))
        }
        here ++ deeper
      case OperApply(Neg,Seq(a)) => oneConnectiveSwaps(a).map(newChild => Neg(newChild))
      case _ => Nil
  }

  // Eliminate implications everywhere in the formula (not just at the top).
  // A right-associative chain p1 -> p2 -> ... -> pn is equivalent to
  // not(p1) or not(p2) or ... or not(p_{n-1}) or pn, so this also covers
  // Implies nodes built with more than two arguments.
  def eliminateImplies(form: Expr): Expr =
    form match {
      case Implies(args) =>
        val elimArgs = args.map(eliminateImplies)
        val negatedPrefix = elimArgs.init.map(a => Neg(a))
        Or(negatedPrefix :+ elimArgs.last*)
      case OperApply(op,args) => OperApply(op,args.map(eliminateImplies))
      case x => x
    }

  // Push negations down to the literals (De Morgan's laws + double negation
  // elimination), producing Negation Normal Form. `negate` tracks whether we
  // are currently under an odd number of negations.
  def toNNF(form: Expr, negate: Boolean = false): Expr = form match {
      case Neg(Seq(f)) => toNNF(f,!negate)
      case And(args) =>
        val newArgs = args.map(a => toNNF(a,negate))
        if negate then Or(newArgs*) else And(newArgs*)
      case Or(args) =>
        val newArgs = args.map(a => toNNF(a,negate))
        if negate then And(newArgs*) else Or(newArgs*)
      case Implies(_) => toNNF(eliminateImplies(form),negate)
      case Lit(true,DBool) => And()
      case Lit(false,DBool) => Or()
      case literal => if negate then Neg(literal) else literal
  }

  // distribute Or over And: (a and b) or c == (a or c) and (b or c).
  private def distributeOr(f1: Expr, f2: Expr): Expr =  (f1, f2) match {
    case (And(args),_) => And(args.map(a => distributeOr(a,f2))*)
    case (_,And(args)) => And(args.map(a => distributeOr(f1,a))*)
    case _ => Or(f1,f2)
  }

  private def distributeOrOverAnd(form: Expr): Expr = form match {
    case Or(args) => args.map(distributeOrOverAnd).reduceLeft(distributeOr)
    case And(args) => And(args.map(distributeOrOverAnd)*)
    case x => x
  }

  // distribute And over Or: (a or b) and c == (a and c) or (b and c).
  private def distributeAnd(f1: Expr, f2: Expr): Expr = (f1, f2) match {
    case (Or(args),_) => Or(args.map(a => distributeAnd(a,f2))*)
    case (_,Or(args)) => Or(args.map(a => distributeAnd(f1,a))*)
    case _ => And(f1,f2)
  }

  private def distributeAndOverOr(form: Expr): Expr = form match {
    case And(args) => args.map(distributeAndOverOr).reduceLeft(distributeAnd)
    case Or(args) => Or(args.map(distributeAndOverOr)*)
    case x => x
  }

  // merge nested And/Or of the same connective into one flat list
  def flatten(form: Expr): Expr = form match {
    case OperApply(op@(And | Or),args) =>
      val flatArgs = args.map(flatten).flatMap {
        case OperApply(`op`,inner) => inner
        case f => List(f)
      }
      OperApply(op,flatArgs)
    case OperApply(op,args) => OperApply(op,args.map(flatten))
    case x => x
  }

  def toCNF(form: Expr): Expr = flatten(distributeOrOverAnd(toNNF(form)))

  private def isLiteral(f: Expr): Boolean = f match {
    case Var(_) => true
    case Neg(Seq(Var(_))) => true // Neg.unapply yields the argument list
    case _ => false
  }

  def isCNF(form: Expr): Boolean =
    def isClause(f: Expr): Boolean = f match {
      case OperApply(Or,lits) => lits.forall(isLiteral)
      case l => isLiteral(l)
    }
    form match {
      case OperApply(And,clauses) => clauses.forall(isClause)
      case f => isClause(f)
    }

  def isDNF(form: Expr): Boolean = {
    def isTerm(f: Expr): Boolean = f match {
      case And(lits) => lits.forall(isLiteral)
      case l => isLiteral(l)
    }
    form match {
      case OperApply(Or,terms) => terms.forall(isTerm)
      case f => isTerm(f)
    }
  }

  def toDNF(form: Expr): Expr = flatten(distributeAndOverOr(toNNF(form)))

  // ── Tseitin transformation ─────────────────────────────────────────────
  // toCNF/toDNF above distribute connectives out, which can blow the formula
  // up exponentially (e.g. a balanced tree of n Ors nested under Ands).
  // Tseitin's transformation avoids that: it introduces one fresh variable
  // per subformula and adds small "definitional" clauses saying that
  // variable is equivalent to the subformula it names, giving a result whose
  // size is linear in the size of the input (at the cost of extra variables,
  // and of being only *equisatisfiable* with the input rather than
  // equivalent to it).

  private def freshVarSupply(taken: Set[String]): () => String =
    var counter = 0
    () =>
      var name = s"t$counter"
      counter += 1
      while taken.contains(name) do
        name = s"t$counter"
        counter += 1
      name

  private def neg(f: Expr): Expr = Neg(f)
  private def clause(lits: Expr*): Expr = Or(lits*)
}
