package info.kwarc.probgen

/**
 * Scala binders for STeX syntax with full HTML rendering support.
 * Replaces the original stex.scala — all toString methods are preserved,
 * toHTML methods added/fixed throughout.
 */

// ---------------------------------------------------------------------------
// Root trait
// ---------------------------------------------------------------------------

trait STeXSyntax {
  def toHTML: String
}

// ---------------------------------------------------------------------------
// Parameters / helpers
// ---------------------------------------------------------------------------

case class SParams(pars: (String, String)*) {
  override def toString =
    if (pars.isEmpty) ""
    else pars.map { case (k, v) => s"$k={$v}" }.mkString("[", ", ", "]")
}

// ---------------------------------------------------------------------------
// Abstract environment base
// ---------------------------------------------------------------------------

abstract class SEnvironment(name: String, level: Int = 0) extends STeXSyntax {
  def args: List[String] = Nil
  def params: SParams = SParams()
  def body: Seq[STeXSyntax]
  override def toString = {
    val argsS = args.map(a => s"{$a}").mkString("")
    val spacebefore = if (level == 1) "\n" else if (level >= 2) "\n%%%%%%%%%\n" else ""
    s"$spacebefore\\begin{$name}$argsS${params}\n${body.mkString("\n")}\n\\end{$name}"
  }
  def toHTML: String = body.map(_.toHTML).mkString("\n")
}

// ---------------------------------------------------------------------------
// Document structure
// ---------------------------------------------------------------------------

case class SDocument(body: List[SFragment]) extends SEnvironment("document", 4) {
  def toStringFull =
    """\documentclass{article}
      |\usepackage{stexlight}
      |""".stripMargin + toString
  override def toHTML: String = body.map(_.toHTML).mkString("\n")
}

object SDocument {
  def apply(t: String, p: SProblem): SDocument = SDocument(List(SFragment(t, List(p))))
}

case class SFragment(title: String, body: List[SProblem]) extends SEnvironment("sfragment", 3) {
  override def args = List(title)
  override def toHTML: String = body.map(_.toHTML).mkString("\n")
}

case class SProblem(intro: STeXSyntax, subproblems: List[SSubproblem]) extends SEnvironment("sproblem", 2) {
  def body = intro :: subproblems
  override def toHTML: String =
    s"""<div class="problem-block"><div class="intro">${intro.toHTML}</div>${subproblems.map(_.toHTML).mkString("\n")}</div>"""
}

case class SSubproblem(pts: Int, question: SText, solution: SSolution) extends SEnvironment("subproblem", 1) {
  override def params = SParams("pts" -> pts.toString)
  def body = List(question, solution)
  override def toHTML: String = {
    val id = this.hashCode.abs.toString
    s"""<div class="subproblem" data-id="$id" data-pts="$pts">
      <span class="pts"><b>[$pts pts]</b></span>
      <span class="question">${question.toHTML}</span>
      <div class="answer-row">
        <input type="text" id="ans-$id" placeholder="Enter your answer…" autocomplete="off"/>
        <button onclick="checkAnswer('$id')">Check</button>
      </div>
      <div class="feedback" id="fb-$id"></div>
      <div id="sol-$id" style="display:none">${solution.body.map(_.toHTML).mkString(" ")}</div>
    </div>"""
  }
}

case class SSolution(testspace: Float, body: List[SText]) extends SEnvironment("solution") {
  override def params = SParams("testspace" -> (testspace.toString + "cm"))
  override def toHTML: String = body.map(_.toHTML).mkString(" ")
}

// ---------------------------------------------------------------------------
// Lists
// ---------------------------------------------------------------------------

abstract class SList(n: String, items: List[SText]) extends SEnvironment(n) {
  def body = items.map(SItem(_))
}

case class SItemize(items: SText*) extends SList("itemize", items.toList) {
  override def toHTML: String =
    "<ul>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("\n") + "</ul>"
}

case class SEnumerate(items: SText*) extends SList("enumerate", items.toList) {
  override def toHTML: String =
    "<ol>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("\n") + "</ol>"
}

case class SItem(body: SText) extends STeXSyntax {
  override def toString = "\\item " + body.toString
  override def toHTML: String = s"<li>${body.toHTML}</li>"
}

case class SCenter(body: Seq[STeXSyntax]) extends SEnvironment("center") {
  override def toHTML: String =
    s"<div style='text-align:center'>${body.map(_.toHTML).mkString(" ")}</div>"
}

// ---------------------------------------------------------------------------
// Tabular
// ---------------------------------------------------------------------------

case class STabular(
  cellHead: SText,
  columnHeads: Seq[SText],
  rowHeads: Seq[SText],
  cells: Seq[(Int, Int, SText)]
) extends SEnvironment("tabular") {

  def makeRow(cs: Seq[SText]): SText =
    SSnippet(cs.head +: cs.tail.flatMap(s => Seq(SText(" & "), s)) :+ SText("\\\\"))

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

  override def toHTML: String = {
    val bs = "border: 1px solid #aaa; padding: 6px 12px;"
    val headerHtml = (cellHead +: columnHeads).map(h =>
      s"<th style='$bs background:#eef0f4; text-align:center'>${h.toHTML}</th>"
    ).mkString
    val rowsHtml = rowHeads.zipWithIndex.map { case (rh, i) =>
      val cellsHtml = (0 until columnHeads.length).map { j =>
        val c = cells.find(c => c._1 == i && c._2 == j).map(_._3).getOrElse(SText(" "))
        s"<td style='$bs text-align:center'>${c.toHTML}</td>"
      }.mkString
      s"<tr><th style='$bs background:#eef0f4'>${rh.toHTML}</th>$cellsHtml</tr>"
    }.mkString
    s"<table style='border-collapse:collapse;margin:15px 0'><thead><tr>$headerHtml</tr></thead><tbody>$rowsHtml</tbody></table>"
  }
}

// ---------------------------------------------------------------------------
// SText trait and concrete types
// ---------------------------------------------------------------------------

trait SText extends STeXSyntax {
  def ++(more: Seq[STeXSyntax]): SText = SSnippet(this +: more)
  // preserve original null-safety from stex.scala
  def +(more: STeXSyntax): SText = if (more == null) this else SSnippet(List(this, more))
}

case class SMath(expr: Expr) extends SText {
  override def toString = "$" + expr.toSTeX + "$"
  override def toHTML: String = s"\\(${expr.toSTeX}\\)"
}

case class SSnippet(body: Seq[STeXSyntax], sep: String = "") extends SText {
  override def toString = body.map(_.toString).mkString(sep)
  override def toHTML: String = body.map(_.toHTML).mkString(sep)
  def +(rest: SSnippet): SSnippet = copy(body = this.body ++ rest.body)
}

case class SPlainText(body: String) extends SText {
  override def toString = body
  override def toHTML: String = body
    .replace("\\uProb",       "P")
    .replace("\\intmax",      "max")
    .replace("\\intmin",      "min")
    .replace("\\intlessthan", " &lt; ")
    .replace("\\intlethan",   " &le; ")
    .replace("\\intgreatthan"," &gt; ")
    .replace("\\intgeq",      " &ge; ")
    .replace("\\intdivisible"," | ")
    .replace("\\nequals",     " &ne; ")
    .replace("\\inset",       " &isin; ")
    .replace("\\range",       "&#8230;")
    .replace("\\\\",          "<br>")
    .replace("\\hline",       "")
    .replace("\\to",          "&rarr;")
    .replace("\\mathtt{up}",    "<code>up</code>")
    .replace("\\mathtt{down}",  "<code>down</code>")
    .replace("\\mathtt{left}",  "<code>left</code>")
    .replace("\\mathtt{right}", "<code>right</code>")
}

// ---------------------------------------------------------------------------
// Macro application
// ---------------------------------------------------------------------------

case class SMacroApplication(name: String, args: Seq[SText], flexary: Boolean) extends SText {

  override def toString: String = {
    val command = "\\" + name
    val argsX   = args.map(_.toString)
    val argsS   = if (flexary) argsX.mkString("{", ",", "}")
                  else argsX.map(s => "{" + s + "}").mkString("")
    command + argsS
  }

  override def toHTML: String = {
    val a = args.map(_.toHTML)
    val n = name.stripPrefix("\\").toLowerCase
    n match {
      // Probability
      case "uprob"      | "condprob" =>
        if (a.isEmpty) "P"
        else if (a.length == 1) s"P(${a(0)})"
        else s"P(${a(0)} | ${a(1)})"
      // Arithmetic — binary/nary
      case "intplus"       => a.mkString(" + ")
      case "intminus"      => a.mkString(" &minus; ")
      case "inttimes"      => a.mkString(" &middot; ")
      case "realdivide"    => if (a.length >= 2) s"${a(0)} / ${a(1)}" else a.mkString(" / ")
      case "intpower"      => if (a.length >= 2) s"${a(0)}<sup>${a(1)}</sup>" else a.mkString("^")
      case "intmod"        => if (a.length >= 2) s"${a(0)} mod ${a(1)}" else a.mkString(" mod ")
      case "intmax"        => if (a.isEmpty) "max" else "max(" + a.mkString(", ") + ")"
      case "intmin"        => if (a.isEmpty) "min" else "min(" + a.mkString(", ") + ")"
      case "intdivisible"  => if (a.length >= 2) s"${a(0)} | ${a(1)}" else a.mkString(" | ")
      // Relational
      case "equals"        => a.mkString(" = ")
      case "nequals"       => a.mkString(" &ne; ")
      case "intlessthan"   => a.mkString(" &lt; ")
      case "intlethan"     => a.mkString(" &le; ")
      case "intgreatthan"  => a.mkString(" &gt; ")
      case "intgeq"        => a.mkString(" &ge; ")
      case "inset"         => if (a.length >= 2) s"${a(0)} &isin; ${a(1)}" else a.mkString(" &isin; ")
      // Logical
      case "lconj"         => a.mkString(" &and; ")
      case "ldisj"         => a.mkString(" &or; ")
      case "implies"       => a.mkString(" &rArr; ")
      case "lneg"          => "&not;" + a.mkString("")
      // Constructors
      case "set"           => "{" + a.mkString(", ") + "}"
      case "tup"           => "(" + a.mkString(", ") + ")"
      case "seq"           => a.mkString(", ")
      case "range"         => if (a.length >= 2) s"${a(0)}&#8230;${a(1)}" else a.mkString("…")
      case "apply"         => a.headOption.map(h => h + "(" + a.drop(1).mkString(", ") + ")").getOrElse("")
      // MDP-specific display
      case "transitions"   =>
        if (a.isEmpty) ""
        else a.head + a.tail.grouped(2).map {
          case Seq(act, st) => s" <span class='transition-arrow'>&xrarr;<sub>$act</sub></span> $st"
          case Seq(st)      => s" &rarr; $st"
          case _            => ""
        }.mkString("")
      // Direction actions rendered as code
      case s if s.startsWith("mathtt") => s"<code>${a.mkString}</code>"
      // Fallback: wrap raw LaTeX in \(...\) so MathJax renders it
      case _ => s"\\(${this.toString}\\)"
    }
  }
}

class SMacro(name: String) {
  def apply(args: SText*) = SMacroApplication(name, args.toList, false)
}

// ---------------------------------------------------------------------------
// String interpolation
// ---------------------------------------------------------------------------

object SText {
  implicit class STextInterpolator(sc: StringContext) {
    def x(args: Any*): SText = {
      var partsS = sc.parts.toList.map { s =>
        val sR = s.replace('§', '$').replace("\\n", "\n")
        SPlainText(sR)
      }
      var snippets: List[STeXSyntax] = List(partsS.head)
      partsS = partsS.tail
      args.toList.foreach { arg =>
        val argS: STeXSyntax = arg match {
          case s: String => SPlainText(s)
          case f: Form   => SMath(f)
          case a =>
            Expr.fromAnyO(a) match {
              case Some(e) => SMath(e)
              case None    => SPlainText(a.toString)
            }
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
