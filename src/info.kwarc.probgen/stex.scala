package info.kwarc.probgen

/**
 * Final fixed Scala binders for stex syntax.
 * Corrects pattern matching priority and adds missing macro translations.
 */

trait STeXSyntax {

  def toHTML: String = this match {

    // 1. HIGHEST PRIORITY: TABULAR
    // Must be first so it doesn't fall through to SText/toString
    case t: STabular =>
      val borderStyle = "border: 1px solid black; padding: 6px;"
      val headerHtml = (t.cellHead +: t.columnHeads).map(h =>
        s"<th style='$borderStyle background-color: #eee;'>${h.toHTML}</th>"
      ).mkString

      val rowsHtml = t.rowHeads.zipWithIndex.map { case (rh, i) =>
        val cells = (0 until t.columnHeads.length).map { j =>
          val content = t.cells.find(c => c._1 == i && c._2 == j).map(_._3).getOrElse(SText(" "))
          s"<td style='$borderStyle text-align: center;'>${content.toHTML}</td>"
        }.mkString
        s"<tr><th style='$borderStyle background-color: #eee;'>${rh.toHTML}</th>$cells</tr>"
      }.mkString

      s"<table style='border-collapse: collapse; margin: 15px 0; border: 1px solid black;'><thead><tr>$headerHtml</tr></thead><tbody>$rowsHtml</tbody></table>"

    // 2. STRUCTURAL ELEMENTS
    case SProblem(intro, subs) =>
      s"""<div class="problem-block"><p>${intro.toHTML}</p>${subs.map(_.toHTML).mkString("\n")}</div>"""

    case SSubproblem(pts, question, _) =>
      s"""<div class="subproblem"><b>[$pts pts]</b><br>${question.toHTML}</div>"""

    // 3. MATH & MACROS
    case m: SMacroApplication => m.toHTML
    case m: SMath => m.toHTML

    // 4. TEXT FALLBACK (Fixes raw strings seen in screenshot)
    case t: SText =>
      t.toString
        .replace("\\uProb", "P")
        .replace("\\intmax", "max")
        .replace("\\intmin", "min")
        .replace("\\intlessthan", " &lt; ")
        .replace("\\intgreatthan", " &gt; ")
        .replace("\\intleq", " &le; ")
        .replace("\\intgeq", " &ge; ")
        .replace("\\intdivisible", " | ")
        .replace("\\nequals", " &ne; ")
        .replace("\\inset", " &isin; ")
        .replace("\\range", " ")
        .replace("\\\\", "<br>")
        .replace("\\hline", "")

    // 5. OTHER
    case SItemize(items @ _*) => "<ul>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("\n") + "</ul>"
    case SEnumerate(items @ _*) => "<ol>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("\n") + "</ol>"
    case SItem(body) => s"<li>${body.toHTML}</li>"
    case SCenter(body) => s"<div style='text-align:center'>${body.map(_.toHTML).mkString("<br>")}</div>"
    case env: SEnvironment => env.body.map(_.toHTML).mkString("\n")
    case other => other.toString
  }
}

/*****************/

case class SParams(pars: (String, String)*) {
  override def toString =
    if (pars.isEmpty) ""
    else pars.map { case (k, v) => s"$k={$v}" }.mkString("[", ", ", "]")
}

abstract class SEnvironment(name: String, level: Int = 0) extends STeXSyntax {
  def args: List[String] = Nil
  def params: SParams = SParams()
  def body: Seq[STeXSyntax]
  override def toString = body.map(_.toString).mkString("\n")
}

case class SDocument(body: List[SFragment]) extends SEnvironment("document", 4) {
  def toStringFull = """\documentclass{article}\n\usepackage{stexlight}\n""" + toString
}

object SDocument {
  def apply(t: String, p: SProblem): SDocument = SDocument(List(SFragment(t, List(p))))
}

case class SFragment(title: String, body: List[SProblem]) extends SEnvironment("sfragment", 3) {
  override def args = List(title)
}

case class SProblem(intro: STeXSyntax, subproblems: List[SSubproblem]) extends SEnvironment("sproblem", 2) {
  def body = intro :: subproblems
}

case class SSubproblem(pts: Int, question: SText, solution: SSolution) extends SEnvironment("subproblem", 1) {
  override def params = SParams("pts" -> pts.toString)
  def body = List(question, solution)
}

case class SSolution(testspace: Float, body: List[SText]) extends SEnvironment("solution") {
  override def params = SParams("testspace" -> (testspace.toString + "cm"))
}

abstract class SList(n: String, items: List[SText]) extends SEnvironment(n) {
  def body = items.map(SItem(_))
}

case class SItemize(items: SText*) extends SList("itemize", items.toList)
case class SEnumerate(items: SText*) extends SList("enumerate", items.toList)

case class SItem(body: SText) extends STeXSyntax {
  override def toString = body.toString
}

case class SCenter(body: Seq[STeXSyntax]) extends SEnvironment("center")

case class STabular(
                     cellHead: SText,
                     columnHeads: Seq[SText],
                     rowHeads: Seq[SText],
                     cells: Seq[(Int, Int, SText)]
                   ) extends SEnvironment("tabular") {

  def makeRow(cs: Seq[SText]): SText =
    SSnippet(cs.flatMap(c => Seq(c, SText(" & "))).dropRight(1) :+ SText("\\\\"))

  override def args = List("l|" + ("c" * columnHeads.length))

  def body = {
    val headerRow = makeRow(cellHead +: columnHeads)
    val bodyRows = rowHeads.zipWithIndex.map { case (r, i) =>
      val values = (0 until columnHeads.length).map { j =>
        cells.find(c => c._1 == i && c._2 == j).map(_._3).getOrElse(SText(" "))
      }
      makeRow(r +: values)
    }
    headerRow +: SText("\\hline") +: bodyRows
  }

  // FIX: override toHTML directly on STabular so it never falls through to
  // SEnvironment.body rendering, regardless of how toHTML is dispatched.
  override def toHTML: String = {
    val borderStyle = "border: 1px solid black; padding: 6px;"
    val headerHtml = (cellHead +: columnHeads).map(h =>
      s"<th style='$borderStyle background-color: #eee;'>${h.toHTML}</th>"
    ).mkString

    val rowsHtml = rowHeads.zipWithIndex.map { case (rh, i) =>
      val cellsHtml = (0 until columnHeads.length).map { j =>
        val content = cells.find(c => c._1 == i && c._2 == j).map(_._3).getOrElse(SText(" "))
        s"<td style='$borderStyle text-align: center;'>${content.toHTML}</td>"
      }.mkString
      s"<tr><th style='$borderStyle background-color: #eee;'>${rh.toHTML}</th>$cellsHtml</tr>"
    }.mkString

    s"<table style='border-collapse: collapse; margin: 15px 0; border: 1px solid black;'><thead><tr>$headerHtml</tr></thead><tbody>$rowsHtml</tbody></table>"
  }
}

trait SText extends STeXSyntax {
  def ++(more: Seq[STeXSyntax]) = SSnippet(this +: more)
  def +(more: STeXSyntax) = SSnippet(List(this, more))
}

case class SMath(expr: Expr) extends SText {
  override def toString = "$" + expr.toSTeX + "$"
  override def toHTML: String = s"\\(${expr.toSTeX}\\)"
}

case class SSnippet(body: Seq[STeXSyntax], sep: String = "") extends SText {
  override def toString = body.map(_.toString).mkString(sep)
  // FIX: render each child with toHTML so SMath/SMacroApplication nodes
  // produce MathJax/HTML output instead of raw LaTeX strings
  override def toHTML: String = body.map(_.toHTML).mkString(sep)
}

case class SPlainText(body: String) extends SText {
  override def toString = body
  // FIX: handle macro replacements on plain text nodes directly,
  // replacing the fragile toString+replace in the trait fallback
  override def toHTML: String = body
    .replace("\\uProb", "P")
    .replace("\\intmax", "max")
    .replace("\\intmin", "min")
    .replace("\\intlessthan", " &lt; ")
    .replace("\\intgreatthan", " &gt; ")
    .replace("\\intleq", " &le; ")
    .replace("\\intgeq", " &ge; ")
    .replace("\\intdivisible", " | ")   // FIX: added missing \intdivisible
    .replace("\\nequals", " &ne; ")
    .replace("\\inset", " &isin; ")
    .replace("\\range", " ")
    .replace("\\\\", "<br>")
    .replace("\\hline", "")
}

case class SMacroApplication(name: String, args: Seq[SText], flexary: Boolean)
  extends SText {

  override def toHTML: String = {
    val a = args.map(_.toHTML)
    // FIX: match on name.toLowerCase and also strip any leading backslash,
    // because the parser sometimes stores "uProb" and sometimes "\uProb"
    val n = name.stripPrefix("\\").toLowerCase
    n match {
      // Probability
      case "uprob"         => if (a.isEmpty) "P" else "P(" + a.mkString(", ") + ")"
      // Arithmetic functions
      case "intmax"        => if (a.isEmpty) "max" else "max(" + a.mkString(", ") + ")"
      case "intmin"        => if (a.isEmpty) "min" else "min(" + a.mkString(", ") + ")"
      case "intplus"       => a.mkString(" + ")
      case "intminus"      => a.mkString(" - ")
      case "inttimes"      => a.mkString(" &middot; ")
      case "intdiv"        => a.mkString(" / ")
      // FIX: added intdivisible — shown red in screenshot as unknown macro
      case "intdivisible"  => if (a.length >= 2) a(0) + " | " + a(1)
      else a.mkString(" | ")
      // Relational operators
      case "intlessthan"   => a.mkString(" &lt; ")
      case "intgreatthan"  => a.mkString(" &gt; ")
      case "intleq"        => a.mkString(" &le; ")
      case "intgeq"        => a.mkString(" &ge; ")
      case "nequals"       => a.mkString(" &ne; ")
      case "equals"        => a.mkString(" = ")
      case "inset"         => a.mkString(" &isin; ")
      // Constructors
      case "tup"           => "(" + a.mkString(", ") + ")"
      case "set"           => "{" + a.mkString(", ") + "}"
      case "apply"         => a.headOption.map(h => h + "(" + a.drop(1).mkString(", ") + ")").getOrElse("")
      // FIX: unknown macros — wrap the LaTeX toString in \(...\) for MathJax
      // so at least the LaTeX is rendered rather than shown as red raw text
      case _               => "\\(" + this.toString + "\\)"
    }
  }

  override def toString: String = {
    val a = args.map(_.toString)
    val n = name.stripPrefix("\\").toLowerCase
    n match {
      case "tup"           => "(" + a.mkString(", ") + ")"
      case "set"           => "\\{ " + a.mkString(", ") + " \\}"
      case "apply"         => a.headOption.map(h => h + "(" + a.drop(1).mkString(", ") + ")").getOrElse("")
      case "equals"        => a.mkString(" = ")
      case "intplus"       => a.mkString(" + ")
      case "intminus"      => a.mkString(" - ")
      case "inttimes"      => a.mkString(" · ")
      case "intdiv"        => a.mkString(" / ")
      case "intlessthan"   => a.mkString(" < ")
      case "intgreatthan"  => a.mkString(" > ")
      case "intleq"        => a.mkString(" ≤ ")
      case "intgeq"        => a.mkString(" ≥ ")
      case "nequals"       => a.mkString(" ≠ ")
      case "inset"         => a.mkString(" ∈ ")
      case "intdivisible"  => if (a.length >= 2) a(0) + " | " + a(1) else a.mkString(" | ")
      case "uprob"         => if (a.isEmpty) "P" else "P(" + a.mkString(", ") + ")"
      case "intmax"        => if (a.isEmpty) "max" else "max(" + a.mkString(", ") + ")"
      case "intmin"        => if (a.isEmpty) "min" else "min(" + a.mkString(", ") + ")"
      case _               => "\\" + name + a.map("{" + _ + "}").mkString
    }
  }
}

object SText {
  implicit class STextInterpolator(sc: StringContext) {
    def x(args: Any*): SText = {
      var partsS = sc.parts.toList.map(s => SPlainText(s.replace('§', '$').replace("\\n", "\n")))
      var snippets: List[STeXSyntax] = List(partsS.head)
      partsS = partsS.tail
      args.foreach { arg =>
        val argS = arg match {
          case s: String => SPlainText(s)
          case f: Form   => SMath(f)
          case other     => SPlainText(other.toString)
        }
        snippets ::= argS
        snippets ::= partsS.head
        partsS = partsS.tail
      }
      SSnippet(snippets.reverse)
    }
  }
  def apply(args: STeXSyntax*): SText = SSnippet(args.toList)
  def apply(s: String): SText = SPlainText(s)
}