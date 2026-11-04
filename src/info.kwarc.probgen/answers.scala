package info.kwarc.probgen

import scala.collection.mutable.ListBuffer

// Small shared helpers

object Html {
  def esc(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

  def codeList(xs: Seq[String]): String =
    xs.map(x => s"<code>${esc(x)}</code>").mkString(", ")
}

object Scoring {
  def roundHalf(d: Double): Double = Math.round(d * 2.0) / 2.0

  /** formats a point value: 2.0 -> "2", 1.5 -> "1.5" */
  def points(d: Double): String =
    if (d == Math.floor(d) && !d.isInfinite && !d.isNaN) d.toInt.toString else d.toString

  /** formats a number for feedback */
  def number(d: Double): String = {
    if (d.isNaN) "NaN"
    else if (d.isInfinite) (if (d > 0) "Infinity" else "-Infinity")
    else if (d == Math.floor(d) && d >= Int.MinValue && d <= Int.MaxValue) d.toInt.toString
    else {
      val s = BigDecimal(d).setScale(6, BigDecimal.RoundingMode.HALF_UP).toString
      s.replaceAll("0+$", "").replaceAll("\\.$", "")
    }
  }

  def plural(n: Int, one: String, many: String): String = if (n == 1) one else many
}

// Answer classes / grading rubric 

/** one criterion of a rubric: something a good answer contains, worth `points` */
case class AnswerClass(points: Int, description: String)

/** a marking scheme; an answer earns the points of every criterion it covers */
case class AnswerRubric(classes: List[AnswerClass], note: String = "") {
  def totalPoints: Int = classes.map(_.points).sum

  /** one checkbox per criterion, each calling `selfGrade` (main.scala) */
  def toHtml(uid: String): String = {
    val items = classes.zipWithIndex.map { case (c, i) =>
      s"<li><label><input type='checkbox' onchange=\"selfGrade('$uid', $i, this.checked)\"/>" +
        s"<span class='rubric-pts'>${c.points} ${Scoring.plural(c.points, "pt", "pts")}</span>" +
        s"<span>${Html.esc(c.description)}</span></label></li>"
    }.mkString("")
    val n = if (note.isEmpty) "" else s"<div class='rubric-note'>${Html.esc(note)}</div>"
    s"<ul class='rubric-list'>$items</ul>$n" +
      s"<div class='rubric-total' id='sg-$uid'>Tick every point your answer covers.</div>"
  }

  def toSTeXBlock: STeXSyntax =
    SItemize(classes.map(c =>
      SText(s"${c.points} ${Scoring.plural(c.points, "pt", "pts")}: ${c.description}"))*)
}

object AnswerRubric {
  /** convenience: AnswerRubric.of(AnswerClass(...), AnswerClass(...)) */
  def of(cs: AnswerClass*): AnswerRubric = AnswerRubric(cs.toList)
}

/**
  * A check result carries how much credit was earned and how to present itself,
  * so that the UI never needs to know the full list of result types.
  * `fraction` is the share of the subproblem's points, in [0,1].
  */
sealed abstract class CheckResult {
  def fraction: Double
  def cssClass: String
  def borderColor: String
  def messageHtml(pts: Int): String
}

case class Correct() extends CheckResult {
  def fraction    = 1.0
  def cssClass    = "correct"
  def borderColor = "#52c97a"
  def messageHtml(pts: Int) = "&#10003; Correct!"
}

case class PartiallyCorrect(fraction: Double, hint: String) extends CheckResult {
  def cssClass    = "partial"
  def borderColor = "#e8a33d"
  def messageHtml(pts: Int) = {
    val got = Scoring.roundHalf(fraction * pts)
    s"&#9681; Partially correct &mdash; <strong>${Scoring.points(got)} of $pts ${Scoring.plural(pts, "point", "points")}</strong>. $hint"
  }
}

case class Incorrect(hint: String) extends CheckResult {
  def fraction    = 0.0
  def cssClass    = "wrong"
  def borderColor = "#e05c5c"
  def messageHtml(pts: Int) = s"&#10007; $hint"
}

/** the input could not be understood at all, not the same as a wrong answer */
case class MalformedInput(hint: String) extends CheckResult {
  def fraction    = 0.0
  def cssClass    = "error"
  def borderColor = "#f0c000"
  def messageHtml(pts: Int) = s"&#9888; $hint"
}

/** fallback: no grader and no rubric, so we can only show the model answer */
case class NotCheckable(expectedSolution: String) extends CheckResult {
  def fraction    = 0.0
  def cssClass    = "review"
  def borderColor = "#4a90d9"
  // expectedSolution is the solution's rendered HTML (from solution().toHTML),
  // so it is inserted as-is rather than escaped
  def messageHtml(pts: Int) =
    s"&#9998; This one is not auto-graded yet. Model answer: <strong>$expectedSolution</strong>"
}

/** the question is not automatically gradable, but we know the grading scheme */
case class NeedsHumanGrading(expected: String, rubric: AnswerRubric, uid: String) extends CheckResult {
  def fraction    = 0.0
  def cssClass    = "review"
  def borderColor = "#4a90d9"
  def messageHtml(pts: Int) =
    s"&#9998; Grade yourself: compare your answer with the model answer and tick what it covers." +
      s"<div class='rubric'><div class='rubric-expected'>Model answer: <strong>$expected</strong></div>" +
      rubric.toHtml(uid) + "</div>"
}

/** the points a student gave themselves with a rubric (see AnswerRubric.toHtml) */
case class SelfGraded(points: Int, outOf: Int) extends CheckResult {
  def fraction    = if (outOf == 0) 0.0 else points.toDouble / outOf
  def cssClass    = if (points >= outOf) "correct" else if (points <= 0) "wrong" else "partial"
  def borderColor = if (points >= outOf) "#52c97a" else if (points <= 0) "#e05c5c" else "#e8a33d"
  def messageHtml(pts: Int) =
    s"You gave yourself <strong>$points of $outOf ${Scoring.plural(outOf, "point", "points")}</strong>."
}

// AnswerParser — check-box and yes/no answers

/**
  * Reads answers that are not expressions (those are read by [[ExprParser]]).
  * Every parser returns Either[String, A]; the Left is the hint shown to the student.
  */
object AnswerParser {
  type Parsed[A] = Either[String, A]

  private val openers  = "([{"
  private val closers  = ")]}"
  private val pairOf   = Map('(' -> ')', '[' -> ']', '{' -> '}')
  private val itemSeps = Set(',', ';')

  private def normalise(s: String): String =
    s.replace(' ', ' ').replaceAll("\\s+", " ").trim

  /**
    * Removes one layer of outer brackets, but only if they genuinely wrap the
    * whole string. "(1,2)" -> "1,2" but "(1,2),(2,3)" is left alone, because
    * there the first bracket closes before the end.
    */
  private def stripWrapper(s: String): String = {
    val t = s.trim
    if (t.length < 2) return t
    if (!pairOf.get(t.charAt(0)).contains(t.charAt(t.length - 1))) return t
    var depth = 0
    var i = 0
    while (i < t.length) {
      val c = t.charAt(i)
      if (openers.indexOf(c.toInt) >= 0) depth += 1
      else if (closers.indexOf(c.toInt) >= 0) {
        depth -= 1
        if (depth == 0 && i < t.length - 1) return t // closed early: not a wrapper
      }
      i += 1
    }
    t.substring(1, t.length - 1).trim
  }

  /** strips only {..} or [..] */
  private def stripBraces(s: String): String = {
    val t = s.trim
    if (t.length >= 2 && ((t.charAt(0) == '{' && t.charAt(t.length - 1) == '}') ||
      (t.charAt(0) == '[' && t.charAt(t.length - 1) == ']'))) stripWrapper(t)
    else t
  }

  /** splits on separators that occur outside any brackets */
  private def splitTop(s: String, seps: Set[Char]): List[String] = {
    val out = ListBuffer[String]()
    val cur = new StringBuilder
    var depth = 0
    s.foreach { c =>
      if (openers.indexOf(c.toInt) >= 0) { depth += 1; cur += c }
      else if (closers.indexOf(c.toInt) >= 0) { depth -= 1; cur += c }
      else if (depth == 0 && seps.contains(c)) { out += cur.toString; cur.clear() }
      else cur += c
    }
    out += cur.toString
    out.toList.map(_.trim).filter(_.nonEmpty)
  }

  /** list separator: commas/semicolons, falling back to whitespace */
  private def splitItems(s: String): List[String] = {
    val byComma = splitTop(s, itemSeps)
    if (byComma.length > 1) byComma else splitTop(s, itemSeps + ' ')
  }

  // ---- booleans -----

  private val trueWords  = Set("t", "true", "y", "yes", "1", "x", "ja", "✓")
  private val falseWords = Set("f", "false", "n", "no", "0", "-", "nein")

  /** "T,F,T,F" · "true, false" · "1,0,1,0" · "yes,no" · "TFTF" */
  def boolSeq(input: String): Parsed[List[Boolean]] = {
    val t = stripBraces(normalise(input))
    if (t.isEmpty) return Left("Please enter an answer, e.g. T,F,T,F.")
    var items = splitItems(t)
    // allow "TFTF" written without separators
    if (items.length == 1 && items.head.length > 1 &&
      items.head.forall(c => "tfTF01".indexOf(c.toInt) >= 0))
      items = items.head.map(_.toString).toList
    Grader.sequence(items) { i =>
      val w = i.toLowerCase
      if (trueWords.contains(w)) Right(true)
      else if (falseWords.contains(w)) Right(false)
      else Left(s"'${Html.esc(i)}' is not a yes/no answer. Use T or F.")
    }
  }

  def yesNo(input: String): Parsed[Boolean] = boolSeq(input) match {
    case Right(b :: Nil) => Right(b)
    case Right(_)        => Left("Give a single yes/no answer.")
    case Left(m)         => Left(m)
  }
}

// Comparing expressions

/**
  * Decides when two expressions mean the same value although they are written
  * differently: numbers by value (1/2 is 0.5), names without font commands (`up` is
  * `\mathtt{up}`), and sets regardless of order and repetition.
  */
object ExprCompare {

  /** the value of an expression that is a number, e.g. 3/5 → 0.6; None for anything else */
  def numericValue(e: Expr): Option[Double] = e match {
    case Lit(s: String, _) => s.trim.toDoubleOption // e.g. the action "-1" of a circular MDP
    case Var(n) if n.trim.toDoubleOption.isDefined => n.trim.toDoubleOption // e.g. R(s) === "-2" in MDP
    case t: Term =>
      try Some(Evaluator.real(t)) catch { case _: EvalError => None }
    case _ => None
  }

  /** the name, without a font command around it: \mathtt{up} → up */
  def nameOf(s: String): String = {
    val t = s.trim
    List("\\mathtt{", "\\mathrm{").find(p => t.startsWith(p) && t.endsWith("}"))
      .map(p => t.stripPrefix(p).stripSuffix("}")).getOrElse(t)
  }

  /** a standard form: two expressions mean the same value iff their standard forms are equal */
  def normalize(e: Expr): Expr = e match {
    case t: Term    => normalizeTerm(t)
    case c: Context => normalizeContext(c)
    case BVar(n)    => Var(nameOf(n))
    case Pred(op, args) => Pred(op, args.map(normalizeTerm))
    case Conn(op, args) => Conn(op, args.map(a => normalize(a).asInstanceOf[Form]))
    case other      => other
  }

  def normalizeTerm(t: Term): Term = numericValue(t) match {
    case Some(v) => DDouble(Math.round(v * 1e9) / 1e9 + 0.0) // + 0.0 turns -0.0 into 0.0
    case None => t match {
      case Var(n)              => Var(nameOf(n))
      case Lit(s: String, _)   => Var(nameOf(s))
      case Apply(FinSet, args) =>
        FinSet(args.map(normalizeTerm).distinct.sortBy(_.toString)*)
      case Apply(op, args)     => Apply(op, args.map(normalizeTerm))
      case other               => other
    }
  }

  def normalizeContext(c: Context): Context =
    Context(c.bindings.map { case (n, v) => nameOf(n) -> normalize(v) })

  /** whether two expressions mean the same value; numbers may differ by `tol` */
  def sameValue(a: Expr, b: Expr, tol: Double = 1e-9): Boolean =
    (numericValue(a), numericValue(b)) match {
      case (Some(x), Some(y)) => Math.abs(x - y) <= tol
      case _                  => normalize(a) == normalize(b)
    }

  /** an expression the way a student would type it, for hints: {a, c}, (0, 1), b'(s)=0.5 */
  def show(e: Expr): String = numericValue(e) match {
    case Some(v) => Scoring.number(v)
    case None => e match {
      case Var(n)                 => nameOf(n)
      case BVar(n)                => nameOf(n)
      case Lit(v, _)              => nameOf(v.toString)
      case BLit(v)                => v.toString
      case c: Context             => c.bindings.map { case (n, v) => s"${nameOf(n)}=${show(v)}" }.mkString(", ")
      case Apply(FinSet, args)    => args.map(show).mkString("{", ", ", "}")
      case Apply(Tuple, args)     => args.map(show).mkString("(", ", ", ")")
      case Apply(FinSeq, args)    => args.map(show).mkString("[", ", ", "]")
      case Apply(FunApply, args)  => show(args.head) + args.tail.map(show).mkString("(", ", ", ")")
      case Conn(Neg, Seq(a))      => "not " + bracketed(a)
      case o: OperApply           => o.args.map(bracketed).mkString(s" ${symbol(o.op)} ")
      case other                  => other.toString
    }
  }

  private def bracketed(e: Expr): String = e match {
    case o: OperApply if !Set[Oper](FinSet, Tuple, FinSeq, FunApply).contains(o.op) => s"(${show(e)})"
    case _ => show(e)
  }

  /** the symbol a student types for an operator (the input syntax of [[ExprParser]]) */
  private def symbol(op: Oper): String = op match {
    case And => "and"; case Or => "or"; case Implies => "->"
    case Equals => "="; case NotEquals => "!="; case Less => "<"; case LessEq => "<="; case InSet => "in"
    case Plus => "+"; case Minus => "-"; case Times => "*"; case Divide => "/"; case Exp => "^"; case Mod => "mod"
    case other => other.stexname
  }
}

// Graders

trait Grader {
  def grade(input: String): CheckResult
}

/** intermediate verdict of a set comparison, so graders can combine several */
case class SetVerdict(fraction: Double, hint: String, exact: Boolean)

object Grader {

  /** lifts a parse result; a parse failure becomes MalformedInput, never "wrong" */
  def onParsed[A](e: Either[String, A])(f: A => CheckResult): CheckResult = e match {
    case Left(m)  => MalformedInput(m)
    case Right(a) => f(a)
  }

  /** all results, or the first error */
  def sequence[A, B](as: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    as.foldRight[Either[String, List[B]]](Right(Nil)) { (a, acc) => for { b <- f(a); bs <- acc } yield b :: bs }

  def toResult(v: SetVerdict): CheckResult =
    if (v.exact) Correct()
    else if (v.fraction <= 0.0) Incorrect(v.hint)
    else PartiallyCorrect(v.fraction, v.hint)

  /**
    * Compares an answer against an expected set.
    *
    * Credit is Jaccard: |A n E| / |A u E|, so both missing and spurious items
    * cost something, and the score is 1.0 exactly when the sets agree.
    *
    * The hint names the extra items (the student wrote those, so nothing is given away) but by default only counts the missing ones, so that partial
    * feedback does not hand over the rest of the answer.
    *
    * `duplicatesMatter` separates the two uses: when listing a set, naming an
    * element twice is harmless; in a sum of probabilities "a + a" really does
    * double-count event a, so each repeat is charged like a spurious item.
    */
  def compareSets[A](answer: List[A], expected: Set[A], show: A => String,
                     revealMissing: Boolean = false, itemName: String = "item",
                     duplicatesMatter: Boolean = false): SetVerdict = {
    val dupes     = answer.diff(answer.distinct).distinct
    val dupCount  = if (duplicatesMatter) answer.length - answer.distinct.length else 0
    val answerSet = answer.toSet
    val inter     = answerSet.intersect(expected)
    val missing   = expected.diff(answerSet)
    val extra     = answerSet.diff(expected)
    val denom     = answerSet.union(expected).size + dupCount
    val frac    = if (denom == 0) 1.0 else inter.size.toDouble / denom.toDouble
    val dupNote =
      if (dupes.isEmpty) ""
      else if (duplicatesMatter)
        s" ${Html.codeList(dupes.map(show).sorted)} appears more than once &mdash; each term may only be counted once."
      else s" You listed ${Html.codeList(dupes.map(show).sorted)} more than once."

    if (missing.isEmpty && extra.isEmpty && dupCount == 0) SetVerdict(1.0, "", true)
    else {
      val parts = ListBuffer[String]()
      parts += s"${inter.size} of ${expected.size} correct"
      if (extra.nonEmpty)
        parts += s"${Html.codeList(extra.toList.map(show).sorted)} " +
          s"${Scoring.plural(extra.size, "is", "are")} not part of the answer"
      if (missing.nonEmpty)
        parts += (if (revealMissing) s"still missing ${Html.codeList(missing.toList.map(show).sorted)}"
                  else s"${missing.size} more to find")
      SetVerdict(frac, parts.mkString("You have ", ", ", ".") + dupNote,
        missing.isEmpty && extra.isEmpty && dupCount == 0)
    }
  }
}


/** a single number; accepts decimals, fractions and percentages alike */
case class NumberGrader(expected: Double, tol: Double = 1e-3, showDirection: Boolean = true) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(NumberGrader.parse(input)) { v =>
    if (Math.abs(v - expected) <= tol) Correct()
    else if (!showDirection) Incorrect("Not quite.")
    else if (v > expected) Incorrect("Not quite &mdash; your value is too large.")
    else Incorrect("Not quite &mdash; your value is too small.")
  }
}

object NumberGrader {
  private val hint = "Enter a number, e.g. 0.6, 3/5 or 60%."

  /** a number as any arithmetic expression; of an equation, the last side counts */
  def parse(input: String): Either[String, Double] = {
    val term: Either[String, Term] = ExprParser.parseTerm(input) match {
      case r @ Right(_) => r
      case Left(err) => ExprParser.parseForm(input) match {
        case Right(Equals(sides)) => Right(sides.last)
        case _                    => Left(err)
      }
    }
    term.flatMap { t =>
      ExprCompare.numericValue(t).toRight(s"'${Html.esc(input.trim)}' is not a number. $hint")
    }
  }
}

// ---- symbolic sums ---------------------------------------------------------

/** "a + c + f" against an expected set of names, with partial credit */
case class TermSumGrader(expected: Set[String], validNames: Set[String] = Set.empty) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(ExprParser.parseTerm(input).flatMap(TermSumGrader.summands)) { terms =>
    TermSumGrader.unknownNames(terms, validNames) match {
      case Some(msg) => MalformedInput(msg)
      case None      =>
        Grader.toResult(Grader.compareSets(terms, expected, (s: String) => s, false, "term", true))
    }
  }
}

object TermSumGrader {
  private val hint = "Write a sum of names, e.g. a + c + f."

  /** the names in a sum such as a + c + f; repetitions are kept so they can be flagged */
  def summands(t: Term): Either[String, List[String]] = {
    def parts(t: Term): List[Term] = t match {
      case Apply(Plus, args) => args.toList.flatMap(parts)
      case other             => List(other)
    }
    Grader.sequence(parts(t)) {
      case Var(n) => Right(n)
      case other  => Left(s"'${Html.esc(ExprCompare.show(other))}' is not a name. $hint")
    }
  }

  /** shared name check, so the student is told about a typo instead of being marked wrong */
  def unknownNames(terms: List[String], validNames: Set[String]): Option[String] = {
    if (validNames.isEmpty) return None
    val unknown = terms.distinct.filterNot(validNames.contains)
    if (unknown.isEmpty) None
    else Some(s"${Html.codeList(unknown.sorted)} ${Scoring.plural(unknown.size, "is not a name", "are not names")} " +
      s"in this problem. Use only: ${Html.codeList(validNames.toList.sorted)}.")
  }
}

/** "(a+c)/(b+d+f)" — numerator and denominator each graded, then averaged */
case class RatioGrader(numerator: Set[String], denominator: Set[String],
                       validNames: Set[String] = Set.empty) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(RatioGrader.parse(input)) { case (n, d) =>
    TermSumGrader.unknownNames(n ::: d, validNames) match {
      case Some(msg) => MalformedInput(msg)
      case None =>
        val vn = Grader.compareSets(n, numerator, (s: String) => s, false, "term", true)
        val vd = Grader.compareSets(d, denominator, (s: String) => s, false, "term", true)
        if (vn.exact && vd.exact) Correct()
        else {
          val frac = (vn.fraction + vd.fraction) / 2.0
          val hint =
            (if (vn.exact) "Numerator is right." else s"Numerator: ${vn.hint}") + " " +
            (if (vd.exact) "Denominator is right." else s"Denominator: ${vd.hint}")
          if (frac <= 0.0) Incorrect(hint) else PartiallyCorrect(frac, hint)
        }
    }
  }
}

object RatioGrader {
  /** (numerator names, denominator names) of e.g. (a+c)/(b+d+f); without brackets,
    * a+c / b+d means a + c/b + d, which is not a quotient
    */
  def parse(input: String): Either[String, (List[String], List[String])] =
    ExprParser.parseTerm(input).flatMap {
      case Apply(Divide, Seq(n, d)) =>
        for { ns <- TermSumGrader.summands(n); ds <- TermSumGrader.summands(d) } yield (ns, ds)
      case _ => Left("Write your answer as a quotient with brackets, e.g. (a+c)/(b+d+f).")
    }
}

// ---- values written as expressions (sets, assignments, labelled values) ----

/** a set of values, e.g. {a, c} or {(1,2), (2,3)}; braces optional, credit as in compareSets */
case class ExprSetGrader(expected: Iterable[Expr], itemName: String = "item",
                         revealMissing: Boolean = false) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(ExprParser.parseTermList(input)) { items =>
    val exp = expected.map(ExprCompare.normalize).toSet
    val ans = items.map(ExprCompare.normalize)
    Grader.toResult(Grader.compareSets(ans, exp, ExprCompare.show, revealMissing, itemName))
  }
}

/** a set of assignments, e.g. a=1, b=2; a=0, b=1; each must bind exactly `variables` (if given) */
case class ContextSetGrader(expected: Iterable[Context], variables: Set[String] = Set.empty) extends Grader {
  def grade(input: String): CheckResult =
    Grader.onParsed(ExprParser.parseContexts(input).flatMap(checkNames)) { cs =>
      val exp: Set[Expr] = expected.map(ExprCompare.normalizeContext).toSet
      val ans: List[Expr] = cs.map(ExprCompare.normalizeContext)
      Grader.toResult(Grader.compareSets(ans, exp, ExprCompare.show, false, "assignment"))
    }

  private def checkNames(cs: List[Context]): Either[String, List[Context]] =
    cs.find(c => variables.nonEmpty && c.asMap.keySet != variables) match {
      case Some(c) => Left(s"Each assignment must give a value to exactly " +
        s"${Html.codeList(variables.toList.sorted)}, but <code>${Html.esc(ExprCompare.show(c))}</code> does not.")
      case None => Right(cs)
    }
}

/** labelled values, e.g. b'(s1) = 0.5, b'(s2) = 0.25; credit per label
  *
  * @param omitZeros labels with value 0 may be left out
  * @param label     applied to the labels on both sides before comparing
  */
case class EquationsGrader(expected: List[(Term, Expr)], tol: Double = 1e-3,
                           omitZeros: Boolean = false, label: Term => Term = identity) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(ExprParser.parseEquations(input)) { answered =>
    def isZero(v: Expr) = ExprCompare.numericValue(v).contains(0.0)
    def keep(eqs: List[(Term, Expr)]) = if (omitZeros) eqs.filterNot(e => isZero(e._2)) else eqs
    val exp = keep(expected).map { case (l, v) => ExprCompare.normalizeTerm(label(l)) -> v }.toMap
    val ansList = keep(answered).map { case (l, v) => ExprCompare.normalizeTerm(label(l)) -> v }
    val ans = ansList.toMap
    if (ans.size < ansList.size) {
      val twice = ansList.groupBy(_._1).collectFirst { case (l, vs) if vs.length > 1 => l }.get
      MalformedInput(s"<code>${Html.esc(ExprCompare.show(twice))}</code> is given more than one value.")
    } else {
      val right   = exp.count { case (l, v) => ans.get(l).exists(a => ExprCompare.sameValue(a, v, tol)) }
      val unknown = ans.keySet.diff(exp.keySet)
      val labels  = exp.keySet.union(ans.keySet)
      if (right == exp.size && unknown.isEmpty) Correct()
      else {
        val parts = ListBuffer(s"$right of ${exp.size} values correct")
        if (unknown.nonEmpty)
          parts += s"${Html.codeList(unknown.toList.map(ExprCompare.show).sorted)} " +
            s"${Scoring.plural(unknown.size, "is", "are")} not asked for"
        val hint = parts.mkString("You have ", ", ", ".")
        val frac = right.toDouble / labels.size.toDouble
        if (frac <= 0.0) Incorrect(hint) else PartiallyCorrect(frac, hint)
      }
    }
  }
}

// ---- booleans / choices ----------------------------------------------------

/**
  * A row of check boxes. Scoring is (right - wrong)/n floored at 0, because
  * plain right/n would award 50% for ticking everything.
  */
case class BoolSeqGrader(expected: List[Boolean]) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(AnswerParser.boolSeq(input)) { ans =>
    if (ans.length != expected.length)
      MalformedInput(s"Give ${expected.length} answers (T or F), separated by commas &mdash; you gave ${ans.length}.")
    else {
      val right = ans.zip(expected).count { case (a, e) => a == e }
      val wrong = expected.length - right
      val frac  = Math.max(0.0, (right - wrong).toDouble / expected.length.toDouble)
      if (wrong == 0) Correct()
      else {
        val hint = s"$right of ${expected.length} boxes correct." +
          (if (frac <= 0.0) " Wrong ticks cancel out right ones, so this scores 0." else "")
        if (frac <= 0.0) Incorrect(hint) else PartiallyCorrect(frac, hint)
      }
    }
  }
}

case class YesNoGrader(expected: Boolean, hint: String = "Not quite.") extends Grader {
  def grade(input: String): CheckResult =
    Grader.onParsed(AnswerParser.yesNo(input)) { b => if (b == expected) Correct() else Incorrect(hint) }
}

// ---- open-ended ------------------------------------------------------------

/**
  * For questions with many correct answers ("give *a* solution", "give *an*
  * inconsistent assignment"): parse the input and run the real solver on it.
  */
case class VerifiedGrader[A](parse: String => Either[String, A],
                             accept: A => Boolean,
                             wrongHint: String) extends Grader {
  def grade(input: String): CheckResult =
    Grader.onParsed(parse(input)) { a => if (accept(a)) Correct() else Incorrect(wrongHint) }
}

/** tries several accepted answer formats and keeps the best outcome */
case class FirstOf(graders: Grader*) extends Grader {
  def grade(input: String): CheckResult = {
    val rs = graders.map(_.grade(input)).toList
    rs.find(_.fraction >= 1.0).getOrElse {
      val usable = rs.filterNot(_.isInstanceOf[MalformedInput])
      if (usable.nonEmpty) usable.maxBy(_.fraction)
      else rs.headOption.getOrElse(MalformedInput("No grader configured for this question."))
    }
  }
}

/**
  * Hands the answer to the student (or a human grader) together with the model answer
  * and the grading scheme; the points come from self-grading (see main.selfGrade).
  */
case class RubricGrader(expected: () => String, rubric: AnswerRubric, uid: String) extends Grader {
  def grade(input: String): CheckResult =
    if (input.trim.isEmpty) MalformedInput("Please enter an answer first.")
    else NeedsHumanGrading(expected(), rubric, uid)
}
