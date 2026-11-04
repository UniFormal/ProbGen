package info.kwarc.probgen

/* We define FOL-signatures and (finite) models.
   For terms/formulas, we reuse the Expr data structures, because those already subsume FOL.
   But the FOL.wff methods allow only the usual the subset that corresponds to FOL syntax.
   This allows future variants where FOL is enriched with some concrete domains.
 */

/** a symbol in a signature */
case class FOLSym(name: String, arity: Int, predicate: Boolean)
/** a signature */
case class FOLSignature(syms: List[FOLSym]) {
  def names = syms.map(_.name)
}

/** the interpretation of a symbol in a model I: I(name)(vars) = body
  * interpretations are given as arbitrary functions in the semantics (= the Expr data structure),
  * predicates are interpreted as Boolean-valued functions, not as relations.
  * FOL.wff checks if a finite model is well-formed by evaluating the function for all arguments and
  * checking that the result is in the universe/a Boolean.
  */
case class FOLSymIntp(name: String, vars: List[String], body: Expr)
/** a finite model with universe {0,...,size-1} */
case class FOLFiniteModel(size: Int, intps: List[FOLSymIntp]) {
  def names = intps.map(_.name)
  def universe = Range(0,size).toList
  def apply(n: String) = intps.find(_.name == n).get
}

object FOL {
  case class Illformed(msg: String) extends Exception
  type Ctx = List[String]
  def wff(sig: FOLSignature): Unit = {
    val ns = sig.names
    if (ns.distinct.length != ns.length) throw Illformed("symbol names not unique")
    sig.syms.foreach {s => if (s.arity < 0) throw Illformed(s"arity of ${s.name} must not be negative")}
  }
  def wff(sig: FOLSignature, ctx: Ctx, tm: Term): Unit = tm match {
    case Var(n) => if (!ctx.contains(n)) throw Illformed(s"variable $n is not in scope")
    case Apply(AbstractFunSym(n),args) =>
      sig.syms.find(s => s.name == n) match {
        case None => throw Illformed(s"no function symbol $n is declared in the signature")
        case Some(sym) =>
          if (sym.predicate) throw Illformed(s"$n is a predicate symbol, not a function symbol")
          if (sym.arity != args.length) throw Illformed(s"$n must take ${sym.arity} arguments")
          args.foreach {a => wff(sig,ctx,a)}
      }
  }
  def wff(sig: FOLSignature, ctx: Ctx, fm: Form): Unit = fm match {
    case Pred(Equals,Seq(l,r)) =>
      wff(sig,ctx,l)
      wff(sig,ctx,r)
    case _: BLit =>
    case Conn(And|Or|Implies, Seq(l,r)) =>
      wff(sig,ctx,l)
      wff(sig,ctx,r)
    case Conn(Neg, Seq(l)) =>
      wff(sig,ctx,l)
    case Pred(AbstractPredSym(n),args) =>
      sig.syms.find(s => s.name == n) match {
        case None => throw Illformed(s"no predicate symbol $n is declared in the signature")
        case Some(sym) =>
          if (!sym.predicate) throw Illformed(s"$n is a function symbol, not a predicate symbol")
          if (sym.arity != args.length) throw Illformed(s"$n must take ${sym.arity} arguments")
          args.foreach {a => wff(sig,ctx,a)}
      }
    case QuantifiedForm(_,n,AbstractDomain,b) => wff(sig, n::ctx, b)
  }

  def generateIntAssignments(vars: List[String], mod: FOLFiniteModel): List[Context] = {
    vars match {
      case Nil => List(Context())
      case v::vs =>
        val as = generateIntAssignments(vs, mod)
        mod.universe.flatMap {i => as.map(a => a(v -> i))}
    }
  }

  def wff(sig: FOLSignature, mod: FOLFiniteModel) = {
    if (mod.size < 0) throw Illformed("a model's universe must have a positive size")
    val syms = sig.syms.sortBy(_.name)
    val intps = mod.intps.sortBy(_.name)
    if (syms.map(_.name) != intps.map(_.name))
      throw Illformed("a model must provide exactly one interpretation for every symbol of the signature")
    (syms zip intps).foreach {case (sym,intp) =>
      if (sym.arity != intp.vars.length)
        throw Illformed("an interpretation must take as many arguments as the arity of the interpreted symbol")
      val as = generateIntAssignments(intp.vars,mod)
      as.foreach {a =>
        (sym.predicate,intp.body) match {
          case (false,t: Term) =>
            val i = Evaluator(t)(using a)
            i match {
              case Lit(i:Int,DInt) if (i>=0 && i<mod.size) =>
              case _ =>
                throw Illformed(s"the interpretation of ${sym.name} is $i, which is not in the universe")
            }
          case (true,f: Form) =>
            try {Evaluator(f)(using a)}
            catch {case _ => throw Illformed(s"the interpretation of ${sym.name} is not a Boolean")}
          case _ => throw Illformed("a function/predicate symbol must be interpreted by universe/Boolean-valued function")
        }
      }
    }
  }

  def interpret(mod: FOLFiniteModel, ass: Context, tm: Term): Lit = tm match {
    case Var(n) => ass(n).asInstanceOf[Lit]
    case Apply(AbstractFunSym(f),args) =>
      val argsI = interpret(mod, ass, args)
      val fI = mod(f)
      Evaluator(fI.body.asInstanceOf[Term])(using Context(fI.vars.zip(argsI)))
  }
  def interpret(mod: FOLFiniteModel, ass: Context, tms: Seq[Term]): Seq[Lit] = tms.map(t => interpret(mod,ass,t))

  def interpret(mod: FOLFiniteModel, ass: Context, fm: Form): BLit = {
    val fmI = fm match {
      case b: BLit => b.value
      case Conn(conn, args) =>
        val argsI = args.map(a => interpret(mod,ass,a))
        Evaluator(conn(argsI*))(using Context())
      case Pred(Equals, Seq(l,r)) =>
        val lI = interpret(mod,ass,l)
        val rI = interpret(mod,ass,r)
        Evaluator(Equals(lI,rI))(using Context())
      case Pred(AbstractPredSym(p),args) =>
        val argsI = interpret(mod, ass, args)
        val pI = mod(p)
        Evaluator(pI.body.asInstanceOf[Form])(using Context(pI.vars.zip(argsI)))
      case QuantifiedForm(u,n,AbstractDomain, b) =>
        val assNs = mod.universe.map(i => ass(n,i))
        val iN = (a: Context) => interpret(mod, a, b) == BLit(true)
        if (u) assNs.forall(iN) else assNs.exists(iN)
    }
    BLit(fmI)
  }
}