package info.kwarc.probgen

// ---------------------------------------------------------------------------
// Helper: converts a LaTeX math string (content between $ $) to HTML
// ---------------------------------------------------------------------------
object MathHTML {
  // Convert a single LaTeX token to HTML
  def token(s: String): String = s.trim match {
    case "\\gamma"  => "<i>γ</i>"
    case "\\pi"     => "<i>π</i>"
    case "\\to"     => "<span class='sym'>→</span>"
    case "\\times"  => "<span class='sym'>×</span>"
    case "\\cdot"   => "<span class='sym'>·</span>"
    case "\\infty"  => "<span class='sym'>∞</span>"
    case "\\hline"  => ""
    case "\\leq"    => "≤"
    case "\\geq"    => "≥"
    case "\\neq"    => "≠"
    case "\\in"     => "∈"
    case cmd if cmd.startsWith("\\mathtt{") && cmd.endsWith("}") =>
      s"<span class='mathtt'>${cmd.stripPrefix("\\mathtt{").stripSuffix("}")}</span>"
    case cmd if cmd.startsWith("\\mathrm{") && cmd.endsWith("}") =>
      cmd.stripPrefix("\\mathrm{").stripSuffix("}")
    case t => t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
  }

  // Convert a LaTeX math string (possibly compound like "4 \times 3") to HTML.
  // Splits on whitespace and maps each token, then joins.
  def apply(s: String): String = {
    val parts = s.trim.split("\\s+")
    if (parts.length <= 1) token(s.trim)
    else parts.map(p => token(p.trim)).mkString(" ")
  }
}

// ---------------------------------------------------------------------------
// Root trait
// ---------------------------------------------------------------------------
trait STeXSyntax {
  def toHTML: String
  def toText: String = ""   // plain text for answer comparison; override where meaningful
}

case class SParams(pars: (String, String)*) {
  override def toString =
    if (pars.isEmpty) ""
    else pars.map { case (k, v) => s"$k={$v}" }.mkString("[", ", ", "]")
}

abstract class SEnvironment(name: String, level: Int = 0) extends STeXSyntax {
  def args: List[String] = Nil
  def params: SParams = SParams()
  def body: Seq[STeXSyntax]
  override def toString = {
    val argsS = args.map(a => s"{$a}").mkString("")
    val sp = if (level == 1) "\n" else if (level >= 2) "\n%%%%%%%%%\n" else ""
    s"$sp\\begin{$name}$argsS${params}\n${body.mkString("\n")}\n\\end{$name}"
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
    s"""<div class="problem-block"><div class="problem-intro">${intro.toHTML}</div>${subproblems.map(_.toHTML).mkString("")}</div>"""
}

case class SSubproblem(pts: Int, question: SText, solution: SSolution, subId: String = "") extends SEnvironment("subproblem", 1) {
  override def params = SParams("pts" -> pts.toString)
  def body = List(question, solution)
  override def toHTML: String = {
    val id = if (subId.nonEmpty) subId else this.hashCode.abs.toString
    s"""<div class="subproblem">
      <div class="subproblem-question"><b>[$pts pts]</b> ${question.toHTML}</div>
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
  override def toText: String = body.map(_.toText).mkString(" ")
}

// ---------------------------------------------------------------------------
// Lists
// ---------------------------------------------------------------------------
abstract class SList(n: String, items: List[SText]) extends SEnvironment(n) {
  def body = items.map(SItem(_))
}
case class SItemize(items: SText*) extends SList("itemize", items.toList) {
  override def toHTML: String =
    "<ul>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("") + "</ul>"
  override def toText: String = items.map(_.toText).mkString(", ")
}
case class SEnumerate(items: SText*) extends SList("enumerate", items.toList) {
  override def toHTML: String =
    "<ol>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("") + "</ol>"
}
case class SItem(body: SText) extends STeXSyntax {
  override def toString = "\\item " + body
  override def toHTML: String = s"<li>${body.toHTML}</li>"
}
case class SCenter(body: Seq[STeXSyntax]) extends SEnvironment("center") {
  override def toHTML: String =
    s"<div style='text-align:center'>${body.map(_.toHTML).mkString("")}</div>"
}

// ---------------------------------------------------------------------------
// Table
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
      val values = Range(0, columnHeads.length).toList
        .map(j => cells.find(c => c._1 == i && c._2 == j).map(_._3).getOrElse(SText(" ")))
      makeRow(r :: values)
    }
    headerRow +: SText("\\hline") +: bodyRows
  }
  override def toHTML: String = {
    val th = "border:1px solid #aaa;padding:6px 12px;background:#eef0f4;font-weight:600;"
    val td = "border:1px solid #aaa;padding:6px 12px;text-align:center;"
    val hdr = (cellHead +: columnHeads)
      .map(h => s"<th style='$th'>${h.toHTML}</th>").mkString("")
    val rows = rowHeads.zipWithIndex.map { case (rh, i) =>
      val tds = Range(0, columnHeads.length).map { j =>
        val c = cells.find(c => c._1 == i && c._2 == j).map(_._3).getOrElse(SText(" "))
        s"<td style='$td'>${c.toHTML}</td>"
      }.mkString("")
      s"<tr><th style='$th'>${rh.toHTML}</th>$tds</tr>"
    }.mkString("")
    s"<table style='border-collapse:collapse;margin:12px 0'><thead><tr>$hdr</tr></thead><tbody>$rows</tbody></table>"
  }
}

// ---------------------------------------------------------------------------
// SText trait
// ---------------------------------------------------------------------------
trait SText extends STeXSyntax {
  def ++(more: Seq[STeXSyntax]): SText = SSnippet(this +: more)
  def +(more: STeXSyntax): SText = if (more == null) this else SSnippet(List(this, more))
}

// ---------------------------------------------------------------------------
// SMath: wraps an Expr, renders as a span.math
// ---------------------------------------------------------------------------
case class SMath(expr: Expr) extends SText {
  override def toString = "$" + expr.toSTeX + "$"
  override def toHTML: String = s"""<span class="math">${expr.toHTML}</span>"""
  override def toText: String = expr.toText
}

// ---------------------------------------------------------------------------
// SSnippet: sequence of nodes, merges adjacent math into one span.math
// ---------------------------------------------------------------------------
case class SSnippet(body: Seq[STeXSyntax], sep: String = "") extends SText {
  override def toString = body.map(_.toString).mkString(sep)
  override def toHTML: String = {
    val sb      = new StringBuilder
    val mathBuf = new StringBuilder

    def flushMath(): Unit =
      if (mathBuf.nonEmpty) {
        sb.append(s"""<span class="math">${mathBuf.toString()}</span>""")
        mathBuf.clear()
      }

    body.foreach {
      case m: SMath =>
        mathBuf.append(m.expr.toHTML)
      case p: SPlainText =>
        val t = p.body.trim
        if (t.startsWith("$") && t.endsWith("$") && t.length > 2 && !t.drop(1).dropRight(1).contains("$")) {
          // Pure §X§ math node — merge into math buffer
          mathBuf.append(MathHTML(t.drop(1).dropRight(1)))
        } else {
          flushMath()
          val html = p.toHTML
          // Add a space when a non-empty text follows non-empty previous content
          // and neither side already has a boundary space
          if (html.nonEmpty && sb.nonEmpty) {
            val lastChar  = sb.last
            val firstChar = html.head
            val needSpace = lastChar != ' ' && firstChar != ' ' &&
              firstChar != '.' && firstChar != ',' && firstChar != ')' &&
              firstChar != ']' && firstChar != '!' && lastChar != '('
            if (needSpace) sb.append(" ")
          }
          sb.append(html)
        }
      case other =>
        flushMath()
        sb.append(other.toHTML)
    }
    flushMath()
    sb.toString()
  }
  override def toText: String = body.map(_.toText).mkString(sep)
  def +(rest: SSnippet): SSnippet = copy(body = this.body ++ rest.body)
}

// ---------------------------------------------------------------------------
// SPlainText
// ---------------------------------------------------------------------------
case class SPlainText(body: String) extends SText {
  override def toString = body
  override def toText: String = {
    // Remove LaTeX math environment markers
    val s = body
      .replace("\\[", "").replace("\\]", "")
      .replace("\\(", "").replace("\\)", "")
      .replace("$", "")
      // Map LaTeX commands to readable text equivalents
      .replace("\\times", "x")
      .replace("\\cdot", "*")
      .replace("\\hline", "")
      .replace("\\to", "->")
      .replace("\\gamma", "γ")
      .replace("\\pi", "π")
      .replace("\\leq", "<=")
      .replace("\\geq", ">=")
      .replace("\\neq", "!=")
      .replace("\\in", "in")
    s.trim
  }
  override def toHTML: String = {
    val parts = body.split("\\$", -1)
    if (parts.length == 1) {
      plainToHTML(body)
    } else {
      val sb = new StringBuilder
      val mb = new StringBuilder
      def flushM(): Unit =
        if (mb.nonEmpty) {
          sb.append(s"""<span class="math">${mb.toString()}</span>""")
          mb.clear()
        }
      parts.zipWithIndex.foreach { case (part, i) =>
        if (i % 2 == 0) {
          if (part.nonEmpty) {
            flushM()
            // Add a space before plain text that follows a math span,
            // unless the text already starts with a space or punctuation
            val needsSpace = sb.nonEmpty && !part.startsWith(" ") &&
              !part.startsWith(".") && !part.startsWith(",") &&
              !part.startsWith(")") && !part.startsWith("]") &&
              !part.startsWith("!")
            if (needsSpace) sb.append(" ")
            sb.append(plainToHTML(part))
          }
        } else {
          mb.append(MathHTML(part))
        }
      }
      flushM()
      sb.toString()
    }
  }

  private def plainToHTML(s: String): String = {
    val t = s.trim
    // If the whole segment is a single LaTeX command, render as inline math
    if (t.startsWith("\\")) {
      val mathHtml = MathHTML.token(t)
      if (mathHtml != t) return s"""<span class="math">$mathHtml</span>"""
    }
    t match {
      case cmd if cmd.startsWith("\\mathtt{") && cmd.endsWith("}") =>
        s"""<span class="math"><span class="mathtt">${cmd.stripPrefix("\\mathtt{").stripSuffix("}")}</span></span>"""
      case cmd if cmd.startsWith("\\mathrm{") && cmd.endsWith("}") =>
        s"""<span class="math">${cmd.stripPrefix("\\mathrm{").stripSuffix("}")}</span>"""
      case _ =>
        // Check if the trimmed text contains only LaTeX tokens (all words start with \)
        // e.g. " 	imes " — render entirely as math
        val words = t.split("\\s+").filter(_.nonEmpty)
        if (words.nonEmpty && words.forall(w => w.startsWith("\\") || w.matches("-?\\d+(\\.\\d+)?"))) {
          val mathContent = words.map(w => MathHTML.token(w)).mkString(" ")
          s"""<span class="math">$mathContent</span>"""
        } else {
          s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
        }
    }
  }
}

// ---------------------------------------------------------------------------
// SMacroApplication
// ---------------------------------------------------------------------------
case class SMacroApplication(name: String, args: Seq[SText], flexary: Boolean) extends SText {
  override def toString = {
    val argsX = args.map(_.toString)
    val argsS = if (flexary) argsX.mkString("{", ",", "}")
    else argsX.map(s => s"{$s}").mkString("")
    s"\\$name$argsS"
  }
  override def toHTML: String = {
    val a = args.map(_.toHTML)
    name match {
      case "uProb"    => s"""<span class="math"><i>P</i>(${a.mkString("")})</span>"""
      case "CondProb" =>
        s"""<span class="math"><i>P</i>(${a.headOption.getOrElse("")}|${a.drop(1).mkString("")})</span>"""
      case _ => toString
    }
  }
  override def toText: String = {
    val a = args.map(_.toText)
    name match {
      case "uProb"    => s"P(${a.mkString("")})"
      case "CondProb" => s"P(${a.headOption.getOrElse("")}|${a.drop(1).mkString("")})"
      case _          => toString
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
        SPlainText(s.replace('§', '$').replace("\\n", "\n"))
      }
      var snippets: List[STeXSyntax] = List(partsS.head)
      partsS = partsS.tail
      args.toList.foreach { arg =>
        val argS: STeXSyntax = arg match {
          case t: STeXSyntax => t
          case s: String     => SPlainText(s)
          case f: Form       => SMath(f)
          case a => Expr.fromAnyO(a) match {
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