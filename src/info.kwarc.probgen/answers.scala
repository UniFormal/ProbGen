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

/**
  * One class of answer, worth `points` of the subproblem's total.
  * The primary purpose is documentation for a human grader (and for a student
  * grading themselves): it records which kind of anwser earns how many points.
  * `detect` is optional. When every class of a rubric has a detector, the rubric
  * can be graded automatically; otherwise the answer is handed to a human.
  * Detectors on free text are inherently unreliable, so leaving them out is fine.
  */
case class AnswerClass(points: Int, description: String, detect: Option[String => Boolean] = None) // add a boolean for traits or class

case class AnswerRubric(classes: List[AnswerClass], note: String = "") {
  def totalPoints: Int = classes.map(_.points).sum
  def fullyDetectable: Boolean = classes.nonEmpty && classes.forall(_.detect.isDefined)

  def toHtml: String = {
    val items = classes.map { c =>
      s"<li><span class='rubric-pts'>${c.points} ${Scoring.plural(c.points, "pt", "pts")}</span>" +
        s"<span>${Html.esc(c.description)}</span></li>"
    }.mkString("")
    val n = if (note.isEmpty) "" else s"<div class='rubric-note'>${Html.esc(note)}</div>"
    s"<ul class='rubric-list'>$items</ul>$n"
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
case class NeedsHumanGrading(expected: String, rubric: AnswerRubric) extends CheckResult {
  def fraction    = 0.0
  def cssClass    = "review"
  def borderColor = "#4a90d9"
  def messageHtml(pts: Int) =
    s"&#9998; Grade yourself against the scheme below." +
      s"<div class='rubric'><div class='rubric-expected'>Model answer: <strong>$expected</strong></div>" +
      rubric.toHtml + "</div>"
}

// AnswerParser — turns what a student typed into values we can compare

/**
  * Every parser returns Either[String, A]; the Left is the hint shown to the
  * student, so parse errors are never wasted. Nothing here throws.
  */
object AnswerParser {
  type Parsed[A] = Either[String, A]

  private val openers  = "([{"
  private val closers  = ")]}"
  private val pairOf   = Map('(' -> ')', '[' -> ']', '{' -> '}')
  private val itemSeps = Set(',', ';')

  // ---- normalisation ----

  /** unifies unicode look-alikes and whitespace; applied by every parser */
  def normalise(s: String): String =
    s.replace('−', '-')   // unicode minus
      .replace('×', '*')  // ×
      .replace('·', '*')  // ·
      .replace('⁄', '/')  // fraction slash
      .replace(' ', ' ')  // non-breaking space
      .replaceAll("\\s+", " ")
      .trim

  /** strips spaces from a label so "b' (1,0)" and "b'(1,0)" compare equal */
  def normLabel(s: String): String = s.replace(" ", "")

  /**
    * Removes one layer of outer brackets, but only if they genuinely wrap the
    * whole string. "(1,2)" -> "1,2" but "(1,2),(2,3)" is left alone, because
    * there the first bracket closes before the end.
    */
  def stripWrapper(s: String): String = {
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

  /** strips only {..} or [..] — used where "(1,2)" must stay a single item */
  def stripBraces(s: String): String = {
    val t = s.trim
    if (t.length >= 2 && ((t.charAt(0) == '{' && t.charAt(t.length - 1) == '}') ||
      (t.charAt(0) == '[' && t.charAt(t.length - 1) == ']'))) stripWrapper(t)
    else t
  }

  /** splits on separators that occur outside any brackets */
  def splitTop(s: String, seps: Set[Char]): List[String] = {
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
  def splitItems(s: String): List[String] = {
    val byComma = splitTop(s, itemSeps)
    if (byComma.length > 1) byComma else splitTop(s, itemSeps + ' ')
  }

  /** index of the last occurrence of `ch` outside any brackets, or -1 */
  private def lastTopIndexOf(s: String, ch: Char): Int = {
    var depth = 0
    var idx   = -1
    var i     = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (openers.indexOf(c.toInt) >= 0) depth += 1
      else if (closers.indexOf(c.toInt) >= 0) depth -= 1
      else if (depth == 0 && c == ch) idx = i
      i += 1
    }
    idx
  }

  private def sequence[A](es: List[Parsed[A]]): Parsed[List[A]] =
    es.collectFirst { case Left(m) => m } match {
      case Some(m) => Left(m)
      case None    => Right(es.map(_.getOrElse(throw new RuntimeException("unreachable"))))
    }

  // ---- numbers -------------------------------------------------------------

  private val LabelPrefix = "^[A-Za-z][A-Za-z0-9_']*\\s*(\\([^)]*\\))?\\s*=\\s*".r
  private val Decimal     = "^[+-]?(\\d+(\\.\\d*)?|\\.\\d+)$".r
  private val NumberHint  = "Enter a number, e.g. 0.6, 3/5 or 60%."

  private def plainNumber(s: String, orig: String): Parsed[Double] = {
    val t = s.trim
    if (t.isEmpty) Left("Please enter an answer.")
    else if (Decimal.matches(t)) t.toDoubleOption match {
      case Some(d) => Right(d)
      case None    => Left(s"'${orig.trim}' is not a number. $NumberHint")
    }
    else Left(s"'${orig.trim}' is not a number. $NumberHint")
  }

  /** accepts 0.6 · .6 · 3/5 · 60% · -2 · "P(x) = 0.6" */
  def number(input: String): Parsed[Double] = {
    var t = normalise(input)
    if (t.isEmpty) return Left("Please enter an answer.")
    if (t.endsWith(".")) t = t.dropRight(1).trim
    t = LabelPrefix.replaceFirstIn(t, "").trim
    t = stripWrapper(t)
    val percent = t.endsWith("%")
    if (percent) t = t.dropRight(1).trim
    val parts = splitTop(t, Set('/'))
    val res =
      if (parts.length == 2)
        for {
          n <- plainNumber(stripWrapper(parts(0)), input)
          d <- plainNumber(stripWrapper(parts(1)), input)
          r <- if (d == 0.0) Left("That is a division by zero.") else Right(n / d)
        } yield r
      else if (parts.length == 1) plainNumber(t, input)
      else Left(s"'${input.trim}' is not a number. $NumberHint")
    res.map(d => if (percent) d / 100.0 else d)
  }

  def numberList(input: String): Parsed[List[Double]] = {
    val t = stripWrapper(normalise(input))
    if (t.isEmpty) return Left("Please enter an answer.")
    val items = splitItems(t)
    if (items.isEmpty) Left("Please enter an answer.") else sequence(items.map(number))
  }

  // ---- integers, sets, tuples ----
  private def integer(s: String): Parsed[Int] = {
    val t = stripWrapper(s.trim)
    t.toIntOption match {
      case Some(i) => Right(i)
      case None    => Left(s"'${t}' is not a whole number.")
    }
  }

  def intList(input: String): Parsed[List[Int]] = {
    val t = stripWrapper(normalise(input))
    if (t.isEmpty) Right(Nil) else sequence(splitItems(t).map(integer))
  }

  def intSet(input: String): Parsed[Set[Int]] = intList(input).map(_.toSet)

  /** a single tuple: "(1,2)" or "1,2" */
  def tuple(input: String): Parsed[List[Int]] = {
    val t = stripWrapper(normalise(input))
    if (t.isEmpty) Left("Enter a tuple such as (1,2).")
    else sequence(splitItems(t).map(integer))
  }

  /** a set of tuples: "{(1,2),(2,3)}" — also accepts it without the braces */
  def tupleSet(input: String): Parsed[Set[List[Int]]] = {
    val t = stripBraces(normalise(input))
    if (t.isEmpty) return Right(Set.empty)
    val items = splitTop(t, itemSeps)
    if (items.isEmpty) Right(Set.empty) else sequence(items.map(tuple)).map(_.toSet)
  }

  // ---- symbolic sums (the Probabilities answer format) --------------------

  private val TermName = "^[A-Za-z][A-Za-z0-9_']*$".r
  private val SumHint  = "Write a sum of names, e.g. a + c + f."

  /** "a + c + f" -> List(a, c, f); duplicates are preserved so a grader can flag them */
  def termSum(input: String): Parsed[List[String]] = {
    val t = stripWrapper(normalise(input).replace(" ", ""))
    if (t.isEmpty) return Left(s"Please enter an answer. $SumHint")
    val parts = splitTop(t, Set('+')).map(stripWrapper)
    if (parts.isEmpty) return Left(s"Please enter an answer. $SumHint")
    parts.find(p => !TermName.matches(p)) match {
      case Some(b) => Left(s"'${b}' is not a valid term. $SumHint")
      case None    => Right(parts)
    }
  }

  /** "(a+c)/(b+d+f)" or "a+c / b+d+f" */
  def ratio(input: String): Parsed[(List[String], List[String])] = {
    val t     = stripWrapper(normalise(input).replace(" ", ""))
    val parts = splitTop(t, Set('/'))
    if (parts.length != 2) Left("Write your answer as a quotient, e.g. (a+c)/(b+d+f).")
    else for { n <- termSum(parts(0)); d <- termSum(parts(1)) } yield (n, d)
  }

  // ---- labelled values -----------------------------------------------------

  /** "a = 0.3, b = 0.2" or "b'(1,0)=0.3, b'(0,0)=0.2" */
  def labelledNumbers(input: String): Parsed[Map[String, Double]] = {
    val t     = stripBraces(normalise(input))
    val items = splitTop(t, itemSeps)
    if (items.isEmpty) return Left("Please enter an answer, e.g. a = 0.3, b = 0.2.")
    val entries = items.map { it =>
      val eq = lastTopIndexOf(it, '=')
      if (eq < 0) Left(s"'${it}' is missing an '='. Write e.g. a = 0.3.")
      else number(it.substring(eq + 1)).map(d => (normLabel(it.substring(0, eq)), d))
    }
    sequence(entries).map(_.toMap)
  }

  /** "a=1, b=2, c=0" — like labelledNumbers but the values must be whole */
  def assignment(input: String): Parsed[Map[String, Int]] =
    labelledNumbers(input).flatMap { m =>
      m.find { case (_, v) => v != Math.floor(v) } match {
        case Some((k, v)) => Left(s"'$k = ${Scoring.number(v)}' must be a whole number.")
        case None         => Right(m.map { case (k, v) => (k, v.toInt) })
      }
    }

  // ---- booleans ------------------------------------------------------------

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
    sequence(items.map { i =>
      val w = i.toLowerCase
      if (trueWords.contains(w)) Right(true)
      else if (falseWords.contains(w)) Right(false)
      else Left(s"'${i}' is not a yes/no answer. Use T or F.")
    })
  }

  def yesNo(input: String): Parsed[Boolean] = boolSeq(input) match {
    case Right(b :: Nil) => Right(b)
    case Right(_)        => Left("Give a single yes/no answer.")
    case Left(m)         => Left(m)
  }

  /** fallback: just the comma/whitespace separated tokens */
  def tokens(input: String): Parsed[List[String]] = {
    val items = splitItems(stripBraces(normalise(input)))
    if (items.isEmpty) Left("Please enter an answer.") else Right(items)
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
  def grade(input: String): CheckResult = Grader.onParsed(AnswerParser.number(input)) { v =>
    if (Math.abs(v - expected) <= tol) Correct()
    else if (!showDirection) Incorrect("Not quite.")
    else if (v > expected) Incorrect("Not quite &mdash; your value is too large.")
    else Incorrect("Not quite &mdash; your value is too small.")
  }
}

/** a vector of numbers, e.g. a belief state or a filtered distribution */
case class NumberListGrader(expected: List[Double], tol: Double = 1e-3, ordered: Boolean = true) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(AnswerParser.numberList(input)) { vs =>
    if (vs.length != expected.length)
      MalformedInput(s"Give ${expected.length} ${Scoring.plural(expected.length, "number", "numbers")}, " +
        s"separated by commas &mdash; you gave ${vs.length}.")
    else {
      val (a, e) = if (ordered) (vs, expected) else (vs.sorted, expected.sorted)
      val right  = a.zip(e).count { case (x, y) => Math.abs(x - y) <= tol }
      if (right == e.length) Correct()
      else if (right == 0) Incorrect(s"None of the ${e.length} values are right yet.")
      else PartiallyCorrect(right.toDouble / e.length, s"$right of ${e.length} values are right.")
    }
  }
}

/** named values, e.g. "b'(1,0) = 0.3, b'(0,0) = 0.2" */
case class LabelledNumberGrader(expected: Map[String, Double], tol: Double = 1e-3) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(AnswerParser.labelledNumbers(input)) { ans =>
    val exp     = expected.map { case (k, v) => (AnswerParser.normLabel(k), v) }
    val right   = exp.count { case (k, v) => ans.get(k).exists(a => Math.abs(a - v) <= tol) }
    val unknown = ans.keySet.diff(exp.keySet)
    val labels  = exp.keySet.union(ans.keySet)
    val frac    = if (labels.isEmpty) 1.0 else right.toDouble / labels.size.toDouble
    if (right == exp.size && unknown.isEmpty) Correct()
    else {
      val parts = ListBuffer[String]()
      parts += s"$right of ${exp.size} values correct"
      if (unknown.nonEmpty)
        parts += s"${Html.codeList(unknown.toList.sorted)} ${Scoring.plural(unknown.size, "is", "are")} not asked for"
      val hint = parts.mkString("You have ", ", ", ".")
      if (frac <= 0.0) Incorrect(hint) else PartiallyCorrect(frac, hint)
    }
  }
}

// ---- sets ------------------------------------------------------------------

/** a set answer with partial credit; see Grader.compareSets for the scoring */
case class SetGrader[A](expected: Set[A],
                        parse: String => Either[String, List[A]],
                        show: A => String,
                        itemName: String = "item",
                        revealMissing: Boolean = false) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(parse(input)) { items =>
    Grader.toResult(Grader.compareSets(items, expected, show, revealMissing, itemName))
  }
}

object SetGrader {
  def ints(expected: Set[Int], itemName: String = "state"): SetGrader[Int] =
    SetGrader[Int](expected, AnswerParser.intList, (i: Int) => i.toString, itemName)

  def strings(expected: Set[String], itemName: String = "item"): SetGrader[String] =
    SetGrader[String](expected, AnswerParser.tokens, (s: String) => s, itemName)

  def tuples(expected: Set[List[Int]], itemName: String = "pair"): SetGrader[List[Int]] =
    SetGrader[List[Int]](expected, s => AnswerParser.tupleSet(s).map(_.toList),
      (t: List[Int]) => t.mkString("(", ",", ")"), itemName)
}

// ---- symbolic sums ---------------------------------------------------------

/** "a + c + f" against an expected set of names, with partial credit */
case class TermSumGrader(expected: Set[String], validNames: Set[String] = Set.empty) extends Grader {
  def grade(input: String): CheckResult = Grader.onParsed(AnswerParser.termSum(input)) { terms =>
    TermSumGrader.unknownNames(terms, validNames) match {
      case Some(msg) => MalformedInput(msg)
      case None      =>
        Grader.toResult(Grader.compareSets(terms, expected, (s: String) => s, false, "term", true))
    }
  }
}

object TermSumGrader {
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
  def grade(input: String): CheckResult = Grader.onParsed(AnswerParser.ratio(input)) { case (n, d) =>
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

/** a small closed set of accepted phrasings, compared ignoring case and punctuation */
case class AnyOf(accepted: Set[String], hint: String = "Not quite.") extends Grader {
  private def key(s: String): String = s.toLowerCase.replaceAll("[^a-z0-9]", "")
  def grade(input: String): CheckResult = {
    val i = key(input)
    if (i.isEmpty) MalformedInput("Please enter an answer.")
    else if (accepted.map(key).contains(i)) Correct()
    else Incorrect(hint)
  }
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
  * Hands the answer to a human (or to the student themselves) together with the grading scheme. If every AnswerClass carries a detector, grades automatically.
  */
case class RubricGrader(expected: () => String, rubric: AnswerRubric) extends Grader {
  def grade(input: String): CheckResult = {
    if (input.trim.isEmpty) return MalformedInput("Please enter an answer first.")
    if (!rubric.fullyDetectable || rubric.totalPoints == 0) return NeedsHumanGrading(expected(), rubric)
    val hit    = rubric.classes.filter(_.detect.get(input))
    val earned = hit.map(_.points).sum
    val total  = rubric.totalPoints
    if (earned >= total) Correct()
    else {
      val hint = s"$earned of $total expected ${Scoring.plural(total, "element", "elements")} found."
      if (earned <= 0) Incorrect(hint) else PartiallyCorrect(earned.toDouble / total.toDouble, hint)
    }
  }
}
