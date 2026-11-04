package info.kwarc.probgen

import scala.collection.mutable.ListBuffer

/** turns what a student typed into an expression tree ([[Expr]]); every entry point returns `Left(message for the student)` if the input cannot be read Precedence, from weakest to strongest, is the same as for rendering ([[Oper.precedence]]): `->`, `or`, `and`, `not`, relations, `+ -`, `mod`, `* /`, `^`. */
object ExprParser {
  type Parsed[A] = Either[String, A]

  def parseForm(input: String): Parsed[Form] = run(input)(_.form())

  def parseTerm(input: String): Parsed[Term] = run(input)(_.term())

  def parseTermList(input: String): Parsed[List[Term]] =
    run(input)(p => p.separated(",")(p.term())).map {
      case List(FinSet(items)) => items.toList
      case List(FinSeq(items)) => items.toList
      case terms               => terms
    }

  def parseEquations(input: String): Parsed[List[(Term, Expr)]] = run(input)(_.equations())

  def parseContext(input: String): Parsed[Context] = parseEquations(input).flatMap(toContext)

  def parseContexts(input: String): Parsed[List[Context]] = {
    val lines = input.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(";")
    if (lines.isEmpty || lines.equalsIgnoreCase("none")) Right(Nil)
    else run(lines)(p => p.separated(";")(p.equations())).flatMap(sequence(_)(toContext))
  }

  // Turning equations into a Context 
  private def toContext(eqs: List[(Term, Expr)]): Parsed[Context] = {
    val pairs = eqs.map {
      case (Var(n), v) => Right(n -> v)
      case (l, _)      => Left(s"The left side of each '=' must be a name, but '$l' is not.")
    }
    sequence(pairs)(identity).flatMap { ps =>
      ps.groupBy(_._1).collectFirst { case (n, vs) if vs.length > 1 => n } match {
        case Some(n) => Left(s"'$n' is given more than one value.")
        case None    => Right(Context(ps))
      }
    }
  }

  /** all results, or the first error */
  private def sequence[A, B](as: List[A])(f: A => Parsed[B]): Parsed[List[B]] =
    as.foldRight[Parsed[List[B]]](Right(Nil)) { (a, acc) => for { b <- f(a); bs <- acc } yield b :: bs }

  // Tokens
  private enum Kind { case Num, Name, Sym, End }

  private case class Token(kind: Kind, text: String)

  private case class ParseError(msg: String) extends Exception(msg)

  private val canonical: Map[String, String] = Map(
    "∧" -> "and", "&" -> "and", "&&" -> "and",
    "∨" -> "or", "||" -> "or",
    "¬" -> "not", "!" -> "not", "~" -> "not", "neg" -> "not",
    "→" -> "->", "⇒" -> "->", "implies" -> "->",
    "≤" -> "<=", "≥" -> ">=", "≠" -> "!=", "<>" -> "!=", "∈" -> "in",
    "×" -> "*", "·" -> "*", "⋅" -> "*", "÷" -> "/", "−" -> "-"
  )

  private val keywords = Set("and", "or", "not", "neg", "implies", "in", "mod", "true", "false")

  private val multiChar = List("->", "<=", ">=", "!=", "<>", "&&", "||")

  private val singleChar = "(){}[],;=<>+-*/^%|&!~⟨⟩"

  private val NumberR = "\\d+(\\.\\d+)?|\\.\\d+".r
  // a name: letters and digits, an optional subscript (_a, _10, _{ab}), optional primes
  private val NameR = "[A-Za-z][A-Za-z0-9]*(_(\\{[A-Za-z0-9]+\\}|[A-Za-z0-9]+))?'*".r
  private val CommandR = "\\\\[A-Za-z]+".r

  private def sym(s: String): Token = Token(Kind.Sym, canonical.getOrElse(s, s))

  private def tokenize(input: String): Vector[Token] = {
    val out = ListBuffer[Token]()
    var i = 0
    while (i < input.length) {
      val rest = input.substring(i)
      val c = input.charAt(i)
      if (c.isWhitespace) i += 1
      else NumberR.findPrefixOf(rest) match {
        case Some(n) => out += Token(Kind.Num, n); i += n.length
        case None => NameR.findPrefixOf(rest) match {
          case Some(n) =>
            if (keywords.contains(n.toLowerCase)) out += sym(n.toLowerCase)
            else out += Token(Kind.Name, n.replace("{", "").replace("}", "")) // D_{a} is D_a
            i += n.length
          case None => CommandR.findPrefixOf(rest) match {
            case Some(cmd) =>
              val name = cmd.drop(1)
              Symbols.unicode(name) match {
                case Some(u) => out += symbolToken(name, u)
                case None    => throw ParseError(s"Unknown command '$cmd'.")
              }
              i += cmd.length
            case None => multiChar.find(rest.startsWith) match {
              case Some(m) => out += sym(m); i += m.length
              case None =>
                // one character, possibly outside the 16-bit range, like 𝒜
                val ch = rest.substring(0, Character.charCount(rest.codePointAt(0)))
                if (singleChar.contains(ch) || canonical.contains(ch)) out += sym(ch)
                else Symbols.command(ch) match {
                  case Some(name) => out += symbolToken(name, ch)
                  case None       => throw ParseError(s"'$ch' is not allowed here.")
                }
                i += ch.length
            }
          }
        }
      }
    }
    (out += Token(Kind.End, "")).toVector
  }

  /** a letter such as γ becomes the name `\gamma` (as the problems write it), an operator its symbol */
  private def symbolToken(command: String, character: String): Token =
    if (Symbols.isIdentifier(character)) Token(Kind.Name, "\\" + command)
    else if (canonical.contains(character) || singleChar.contains(character)) sym(character)
    else throw ParseError(s"'$character' cannot be used in an answer.")


  private def run[A](input: String)(parse: Parser => A): Parsed[A] =
    try {
      val tokens = tokenize(input)
      if (tokens.length == 1) Left("Please enter an answer.")
      else {
        val p = new Parser(tokens)
        val result = parse(p)
        if (!p.atEnd) p.fail("Unexpected text")
        Right(result)
      }
    } catch { case ParseError(msg) => Left(msg) }

  // The parser: one method per precedence level ───────────────────────────

  private final class Parser(tokens: Vector[Token]) {
    private var i = 0

    private def peek: Token = tokens(i)
    def atEnd: Boolean = peek.kind == Kind.End
    private def isSym(s: String): Boolean = peek.kind == Kind.Sym && peek.text == s
    private def accept(s: String): Boolean = if (isSym(s)) { i += 1; true } else false
    private def expect(s: String): Unit = if (!accept(s)) fail(s"Expected '$s'")

    def fail(msg: String): Nothing =
      if (atEnd) throw ParseError(s"$msg, but the answer ends there.")
      else throw ParseError(s"$msg, but found '${peek.text}'.")

    /** one or more items with a separator between them (repeated or trailing separators are ignored) */
    def separated[A](sep: String)(item: => A): List[A] = {
      val items = ListBuffer(item)
      while (accept(sep)) {
        while (accept(sep)) {}
        if (!atEnd) items += item
      }
      items.toList
    }

    // formulas: -> (weakest), or, and, not, then relations/atoms

    def form(): Form = implies()

    private def implies(): Form = {
      val left = or()
      if (accept("->")) Implies(left, implies()) else left // right-associative: a -> b -> c is a -> (b -> c)
    }

    private def or(): Form = {
      val first = and()
      if (!isSym("or")) first
      else { val fs = ListBuffer(first); while (accept("or")) fs += and(); Or(fs.toList*) }
    }

    private def and(): Form = {
      val first = not()
      if (!isSym("and")) first
      else { val fs = ListBuffer(first); while (accept("and")) fs += not(); And(fs.toList*) }
    }

    private def not(): Form = if (accept("not")) Neg(not()) else atom()

    private def atom(): Form =
      if (accept("true")) BLit(true)
      else if (accept("false")) BLit(false)
      else if (isSym("(")) bracketedForm().getOrElse(relation())
      else relation()

    /** `( formula )`, or None (position reset) if the brackets belong to a term, as in `(a + b) < c` */
    private def bracketedForm(): Option[Form] = {
      val start = i
      try {
        expect("(")
        val f = form()
        expect(")")
        if (continuesTerm) { i = start; None } else Some(f)
      } catch { case _: ParseError => i = start; None }
    }

    private val relations = Set("=", "!=", "<", "<=", ">", ">=", "in")
    private def continuesTerm: Boolean =
      peek.kind == Kind.Sym && (relations ++ Set("+", "-", "*", "/", "^", "%", "mod")).contains(peek.text)

    /** a term, or terms connected by relations; a single name on its own is a Boolean variable */
    private def relation(): Form = {
      val args = ListBuffer(term())
      val ops = ListBuffer[String]()
      while (peek.kind == Kind.Sym && relations.contains(peek.text)) {
        ops += peek.text; i += 1
        args += term()
      }
      if (ops.isEmpty) args.head match {
        case Var(n) => BVar(n)
        case _      => fail("Expected a formula (such as p and q, or x = 1)")
      }
      else buildRelation(args.toList, ops.toList)
    }

    /** `a < b < c` is one chained relation; mixed chains become a conjunction of pairs.
      * There are no "greater" operators, so `a > b` is stored as `b < a`.
      */
    private def buildRelation(args: List[Term], ops: List[String]): Form = {
      def pair(op: String, a: Term, b: Term): Form = op match {
        case "="  => Equals(a, b)
        case "!=" => NotEquals(a, b)
        case "<"  => Less(a, b)
        case "<=" => LessEq(a, b)
        case ">"  => Less(b, a)
        case ">=" => LessEq(b, a)
        case _    => InSet(a, b)
      }
      if (ops.distinct.length == 1) ops.head match {
        case "="  => Equals(args*)
        case "<"  => Less(args*)
        case "<=" => LessEq(args*)
        case ">"  => Less(args.reverse*)
        case ">=" => LessEq(args.reverse*)
        case op if args.length == 2 => pair(op, args(0), args(1))
        case op   => And(args.zip(args.tail).map { case (a, b) => pair(op, a, b) }*)
      }
      else And(ops.zip(args.zip(args.tail)).map { case (op, (a, b)) => pair(op, a, b) }*)
    }

    // terms: + - (weakest), mod, * /, unary minus, ^, %, then numbers/names/brackets

    def term(): Term = additive()

    private def additive(): Term = {
      var acc = modulo()
      var sum: List[Term] = Nil // a + b + c becomes one Plus
      while (isSym("+") || isSym("-")) {
        if (accept("+")) {
          sum = (if (sum.isEmpty) List(acc) else sum) :+ modulo()
          acc = Plus(sum*)
        } else {
          expect("-")
          acc = Minus(acc, modulo())
          sum = Nil
        }
      }
      acc
    }

    private def modulo(): Term = {
      var acc = multiplicative()
      while (accept("mod")) acc = Mod(acc, multiplicative())
      acc
    }

    private def multiplicative(): Term = {
      var acc = unary()
      var product: List[Term] = Nil // a * b * c becomes one Times
      while (isSym("*") || isSym("/")) {
        if (accept("*")) {
          product = (if (product.isEmpty) List(acc) else product) :+ unary()
          acc = Times(product*)
        } else {
          expect("/")
          acc = Divide(acc, unary())
          product = Nil
        }
      }
      acc
    }

    private def unary(): Term =
      if (accept("-")) unary() match {
        case Lit(v: Int, DInt)       => DInt(-v)
        case Lit(v: Double, DDouble) => DDouble(-v)
        case t                       => Times(DInt(-1), t) // there is no negation operator
      }
      else if (accept("+")) unary()
      else power()

    private def power(): Term = {
      val base = percent()
      if (accept("^")) Exp(base, unary()) else base // right-associative: 2^3^2 is 2^(3^2)
    }

    private def percent(): Term = {
      val t = primary()
      if (accept("%")) Divide(t, DInt(100)) else t
    }

    private def primary(): Term = peek.kind match {
      case Kind.Num =>
        val n = peek.text; i += 1
        if (n.contains(".")) DDouble(n.toDouble)
        else n.toIntOption.map(DInt(_)).getOrElse(fail("That number is too large"))
      case Kind.Name =>
        val n = peek.text; i += 1
        if (n == "P" && isSym("(")) probability()
        else if (isSym("(")) { expect("("); val args = separated(",")(term()); expect(")"); Var(n)(args*) }
        else Var(n)
      case _ =>
        if (accept("(")) {
          val items = separated(",")(term())
          expect(")")
          if (items.length == 1) items.head else Tuple(items*)
        }
        else if (accept("⟨")) { val items = separated(",")(term()); expect("⟩"); Tuple(items*) }
        else if (accept("{")) {
          if (accept("}")) FinSet()
          else { val items = separated(",")(term()); expect("}"); FinSet(items*) }
        }
        else if (accept("[")) { val items = separated(",")(term()); expect("]"); FinSeq(items*) }
        else fail("Expected a number, a name or a bracket")
    }

    /** P(X=x, Y=y | Z=z): formulas, optionally followed by | and the conditions */
    private def probability(): Term = {
      expect("(")
      val of = separated(",")(form())
      val conds = if (accept("|")) separated(",")(form()) else Nil
      expect(")")
      Prob(of, conds)
    }

    // equations: left = right, separated by commas

    def equations(): List[(Term, Expr)] = separated(",")(equation())

    private def equation(): (Term, Expr) = {
      val left = term()
      expect("=")
      val right: Expr =
        if (accept("true")) BLit(true) else if (accept("false")) BLit(false) else term()
      (left, right)
    }
  }
}
