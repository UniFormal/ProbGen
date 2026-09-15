package info.kwarc.probgen

object PropLogic {
  def assignments(vs: Seq[String]): List[Context] = vs match {
    case Nil => Nil
    case h :: t =>
      val tA = assignments(t)
      tA.flatMap {c => List(c(h -> true),c(h -> false))}
  }

  def findassignment(form: Form,satisfying: Boolean): Option[Context] = findAllAssignments(form,satisfying).headOption

  def findAllAssignments(form: Form, satisfying: Boolean): Seq[Context] = {
    val collected = form.fvsD
    assignments(collected).filter {ctx =>
      val eval = Evaluator(form)(using ctx)
      if satisfying then eval else !eval
    }
  }

  // Two formulas are (semantically) equivalent iff they agree on every truth
  // assignment over the union of the variables occurring in either of them.
  def isEquivalent(f1: Form,f2: Form): Boolean = {
    val vars = (f1.fvs ++ f2.fvs).distinct
    assignments(vars).forall(ctx =>
      Evaluator(f1)(using ctx) == Evaluator(f2)(using ctx)
    )
  }

  // A formula is valid (a tautology) iff no assignment falsifies it.
  def isValid(form: Form): Boolean = findAllAssignments(form,false).isEmpty

  // Every formula reachable from `form` by changing exactly one And/Or/
  // Implies node into a *different* one of those three connectives, keeping
  // its children untouched (Neg and the leaves are left alone - there's no
  // same-arity "different connective" to swap a negation or a variable for).
  // Used for "this formula is invalid, but one connective away from valid"
  // exercises.
  def oneConnectiveSwaps(form: Form): List[Form] = {
    val alternatives = List(And,Or,Implies)
    form match
      case Conn(op,args) if alternatives.contains(op) =>
        val here = alternatives.filter(_ != op).map(alt => Conn(alt,args))
        val deeper = args.indices.toList.flatMap {i =>
          oneConnectiveSwaps(args(i)).map(newChild => Conn(op,args.updated(i,newChild)))
        }
        here ++ deeper
      case Conn(Neg,Seq(a)) => oneConnectiveSwaps(a).map(newChild => Conn(Neg,List(newChild)))
      case _ => Nil
  }

  // Eliminate implications everywhere in the formula (not just at the top).
  // A right-associative chain p1 -> p2 -> ... -> pn is equivalent to
  // not(p1) or not(p2) or ... or not(p_{n-1}) or pn, so this also covers
  // Implies nodes built with more than two arguments.
  def eliminateImplies(form: Form): Form =
    form match {
      case Implies(args) =>
        val elimArgs = args.map(eliminateImplies)
        val negatedPrefix = elimArgs.init.map(a => Neg(a))
        Or(negatedPrefix :+ elimArgs.last*)
      case Conn(op,args) => Conn(op,args.map(eliminateImplies))
      case x => x
    }

  // Push negations down to the literals (De Morgan's laws + double negation
  // elimination), producing Negation Normal Form. `negate` tracks whether we
  // are currently under an odd number of negations.
  def toNNF(form: Form, negate: Boolean = false): Form = form match {
      case Neg(Seq(f)) => toNNF(f,!negate)
      case And(args) =>
        val newArgs = args.map(a => toNNF(a,negate))
        if negate then Or(newArgs*) else And(newArgs*)
      case Or(args) =>
        val newArgs = args.map(a => toNNF(a,negate))
        if negate then And(newArgs*) else Or(newArgs*)
      case Implies(_) => toNNF(eliminateImplies(form),negate)
      case BLit(true) => And()
      case BLit(false) => Or()
      case literal => if negate then Neg(literal) else literal
  }

  // distribute Or over And: (a and b) or c == (a or c) and (b or c).
  private def distributeOr(f1: Form, f2: Form): Form =  (f1, f2) match {
    case (And(args),_) => And(args.map(a => distributeOr(a,f2))*)
    case (_,And(args)) => And(args.map(a => distributeOr(f1,a))*)
    case _ => Or(f1,f2)
  }

  private def distributeOrOverAnd(form: Form): Form = form match {
    case Or(args) => args.map(distributeOrOverAnd).reduceLeft(distributeOr)
    case And(args) => And(args.map(distributeOrOverAnd)*)
    case x => x
  }

  // distribute And over Or: (a or b) and c == (a and c) or (b and c).
  private def distributeAnd(f1: Form, f2: Form): Form = (f1, f2) match {
    case (Or(args),_) => Or(args.map(a => distributeAnd(a,f2))*)
    case (_,Or(args)) => Or(args.map(a => distributeAnd(f1,a))*)
    case _ => And(f1,f2)
  }

  private def distributeAndOverOr(form: Form): Form = form match {
    case And(args) => args.map(distributeAndOverOr).reduceLeft(distributeAnd)
    case Or(args) => Or(args.map(distributeAndOverOr)*)
    case x => x
  }

  // merge nested And/Or of the same connective into one flat list
  private def flatten(form: Form): Form = form match {
    case Conn(op@(And | Or),args) =>
      val flatArgs = args.map(flatten).flatMap {
        case Conn(`op`,inner) => inner
        case f => List(f)
      }
      Conn(op,flatArgs)
    case Conn(op,args) => Conn(op,args.map(flatten))
    case x => x
  }

  def toCNF(form: Form): Form = flatten(distributeOrOverAnd(toNNF(form)))

  private def isLiteral(f: Form): Boolean = f match {
    case BVar(_) => true
    case Neg(BVar(_)) => true
    case _ => false
  }

  def isCNF(form: Form): Boolean =
    def isClause(f: Form): Boolean = f match {
      case Conn(Or,lits) => lits.forall(isLiteral)
      case l => isLiteral(l)
    }
    form match {
      case Conn(And,clauses) => clauses.forall(isClause)
      case f => isClause(f)
    }

  def isDNF(form: Form): Boolean = {
    def isTerm(f: Form): Boolean = f match {
      case And(lits) => lits.forall(isLiteral)
      case l => isLiteral(l)
    }
    form match {
      case Conn(Or,terms) => terms.forall(isTerm)
      case f => isTerm(f)
    }
  }

  def toDNF(form: Form): Form = flatten(distributeAndOverOr(toNNF(form)))

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

  private def neg(f: Form): Form = Conn(Neg, List(f))
  private def clause(lits: Form*): Form = Conn(Or, lits.toList)

  def parseForm(input: String): Form = FormulaParser.parse(input)
}

object FormulaParser {
  def parse(input: String): Form =
    val tokens = tokenize(input)
    val parser = new Parser(tokens)
    val result = parser.parseFormula()
    if (!parser.atEnd) then
      throw new IllegalArgumentException(s"Unexpected token : ${parser.peek}")
    result

  private def tokenize(input: String): List[String] =
    input
      .replace("(", " ( ")
      .replace(")", " ) ")
      .replace("->", " -> ")
      .trim
      .split("\\s+")
      .toList
      .filter(_.nonEmpty)
  private class Parser(tokens: List[String]) {
    private var position = 0

    def atEnd: Boolean =
      position >= tokens.length

    def peek: String =
      if (atEnd) "<EOF>" else tokens(position)

    private def consume(): String =
      val token = peek
      position += 1
      token

    private def expect(token: String): Unit =
      if (peek != token)
        throw new IllegalArgumentException(s"Expected '$token' , found '$peek'")
      consume()

    def parseFormula(): Form =
      parseImplies()

    private def parseImplies(): Form =
      var left = parseOr()

      if (peek == "->" || peek == "implies") then
        consume()
        val right = parseImplies()
        Conn(Implies, List(left, right))
      else left

    private def parseOr(): Form =
      val first = parseAnd()
      if (peek != "or") then first
      else
        val forms = scala.collection.mutable.ListBuffer(first)
        while (peek == "or")
          consume()
          forms += parseAnd()
        Conn(Or, forms.toList)

    private def parseAnd(): Form =
      var left = parseNeg()
      if (peek != "and") then left
      else
        val forms = scala.collection.mutable.ListBuffer(left)
        while (peek == "and") do
          consume()
          forms += parseNeg()
        Conn(And, forms.toList)

    private def parseNeg(): Form =
      // accept both "neg" and "not" - Neg.text (used when rendering, i.e.
      // what's shown to users as e.g. a problem's solution) renders as
      // "not", so the parser must accept that spelling too or its own
      // output wouldn't parse back in
      if (peek == "neg" || peek == "not") then
        consume()
        Conn(Neg, List(parseNeg()))
      else parseAtom()

    private def parseAtom(): Form =
      if (peek == "(") then
        consume()
        val formula = parseFormula()
        expect(")")
        formula
      else if (peek.matches("[A-Za-z][A-Za-z0-9_]*")) then BVar(consume())
      else
        throw new IllegalArgumentException(
          s"Expected Variable or '(', found '$peek' "
        )

  }
}

object Mytest {

  def main(args: Array[String]) =
    val k = PropLogic.parseForm("p -> q -> p")
    println(k)
    val gk = PropLogic.parseForm("(p -> q) -> p")
    println(gk)
    val ans = PropLogic.findAllAssignments(k, true)
    val ans2 = PropLogic.findAllAssignments(gk, true)
    ans.foreach(println)
    println("=======================")
    ans2.foreach(println)

    println("=======================")
    val cnfSrc = PropLogic.parseForm("(p -> q) -> p")
    val cnf = PropLogic.toCNF(cnfSrc)
    println(s"${cnfSrc}  ==CNF==>  ${cnf}")

    println("=======================")
    val f1 = PropLogic.parseForm("p -> q")
    val f2 = PropLogic.parseForm("neg p or q")
    val f3 = PropLogic.parseForm("neg (p and neg q)")
    println(s"(p -> q) equiv (neg p or q)?          ${PropLogic.isEquivalent(f1, f2)}")
    println(s"(p -> q) equiv (neg (p and neg q))?   ${PropLogic.isEquivalent(f1, f3)}")
    println(s"(p -> q) equiv (p and q)?             ${PropLogic.isEquivalent(f1, PropLogic.parseForm("p and q"))}")

    println("=======================")
    val dnfSrc = PropLogic.parseForm("(p and q) or (neg r and s)")
    val dnf = PropLogic.toDNF(dnfSrc)
    println(s"${dnfSrc}  ==DNF==>  ${dnf}")
    println(s"still equivalent to original? ${PropLogic.isEquivalent(dnfSrc, dnf)}")

    println("=======================")
    for (_ <- 1 to 5)
      val rf = PropFormulaGenerator.generate(
        vars = List("p", "q", "r"),
        minDepth = 2,
        maxDepth = 4,
        minVars = 2,
        weights = ConnectiveWeights(and = 40, or = 20, implies = 30, not = 10)
      )
      println(rf)

    println("=======================")
    val problem = LogicProblemGenerator.make()
    val subs = problem.chooseSubproblems()
    subs.foreach { sub =>
      println(s"[${sub.id}] ${sub.question()}")
      println(s"  expected: ${sub.solution()}")
    }
    val cnfSub = subs.find(_.id == "cnf").get
    println(s"  submit expected solution -> ${cnfSub.checkSolution(cnfSub.solution().toString)}")
    println(s"  submit garbage 'p and'    -> ${cnfSub.checkSolution("p and")}")
    println(s"  submit 'p or not p'       -> ${cnfSub.checkSolution("p or neg p")}")

}
