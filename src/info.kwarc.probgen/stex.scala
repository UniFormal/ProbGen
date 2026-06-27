package info.kwarc.probgen

trait STeXSyntax {
  def toHTML: String
}

case class SParams(pars: (String,String)*) {
  override def toString = {
    if (pars.isEmpty) "" else
      pars.map {case (key,value) => s"$key={$value}"}.mkString("[", ", ", "]")
  }
}

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

case class SDocument(body: List[SFragment]) extends SEnvironment("document", 4) {
  def toStringFull = """\documentclass{article}
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
    s"""<div class="problem-block"><div class="problem-intro">${intro.toHTML}</div>${subproblems.map(_.toHTML).mkString("\n")}</div>"""
}

case class SSubproblem(pts: Int, question: SText, solution: SSolution) extends SEnvironment("subproblem", 1) {
  override def params = SParams("pts" -> pts.toString)
  def body = List(question, solution)
  override def toHTML: String = {
    val id = this.hashCode.abs.toString
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
}

abstract class SList(n: String, items: List[SText]) extends SEnvironment(n) {
  def body = items.map(SItem(_))
}
case class SItemize(items: SText*) extends SList("itemize", items.toList) {
  override def toHTML: String = "<ul>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("") + "</ul>"
}
case class SEnumerate(items: SText*) extends SList("enumerate", items.toList) {
  override def toHTML: String = "<ol>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("") + "</ol>"
}
case class SItem(body: SText) extends STeXSyntax {
  override def toString  = "\\item " + body
  override def toHTML: String = s"<li>${body.toHTML}</li>"
}
case class SCenter(body: Seq[STeXSyntax]) extends SEnvironment("center") {
  override def toHTML: String =
    s"<div style='text-align:center'>${body.map(_.toHTML).mkString("")}</div>"
}

case class STabular(cellHead: SText, columnHeads: Seq[SText], rowHeads: Seq[SText], cells: Seq[(Int,Int,SText)]) extends SEnvironment("tabular") {
  def makeRow(cs: Seq[SText]): SText =
    SSnippet(cs.head +: cs.tail.flatMap(s => Seq(SText(" & "), s)) :+ SText("\\\\"))
  override def args = List("l|" + ("c" * columnHeads.length))
  def body = {
    val headerRow = makeRow(cellHead +: columnHeads)
    val bodyRows = rowHeads.zipWithIndex.map { case (r,i) =>
      val values = Range(0, columnHeads.length).toList
        .map(j => cells.find(c => c._1==i && c._2==j).map(_._3).getOrElse(SText(" ")))
      makeRow(r :: values)
    }
    headerRow +: SText("\\hline") +: bodyRows
  }
  override def toHTML: String = {
    val thStyle = "border:1px solid #aaa;padding:6px 12px;background:#eef0f4;font-weight:600;"
    val tdStyle = "border:1px solid #aaa;padding:6px 12px;text-align:center;"
    val header = (cellHead +: columnHeads).map(h =>
      s"<th style='$thStyle'>${h.toHTML}</th>").mkString("")
    val rows = rowHeads.zipWithIndex.map { case (rh, i) =>
      val tds = Range(0, columnHeads.length).map { j =>
        val c = cells.find(c => c._1==i && c._2==j).map(_._3).getOrElse(SText(" "))
        s"<td style='$tdStyle'>${c.toHTML}</td>"
      }.mkString("")
      s"<tr><th style='$thStyle'>${rh.toHTML}</th>$tds</tr>"
    }.mkString("")
    s"<table style='border-collapse:collapse;margin:12px 0'><thead><tr>$header</tr></thead><tbody>$rows</tbody></table>"
  }
}

trait SText extends STeXSyntax {
  def ++(more: Seq[STeXSyntax]): SText = SSnippet(this +: more)
  def +(more: STeXSyntax): SText = if (more == null) this else SSnippet(List(this, more))

  // Returns MathML content string (no outer <math> tag).
  // Used by SSnippet to merge consecutive math nodes into one <math> block.
  def mathContent: Option[String] = None
}

// Wraps an Expr — renders as a single inline <math> block
case class SMath(expr: Expr) extends SText {
  override def toString = "$" + expr.toSTeX + "$"
  override def mathContent: Option[String] = Some(expr.toHTML)
  override def toHTML: String =
    s"""<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">${expr.toHTML}</math>"""
}

// SSnippet merges consecutive math-only children into one <math> block.
// This prevents "3 <math>\times</math> 3" appearing as separate islands.
case class SSnippet(body: Seq[STeXSyntax], sep: String = "") extends SText {
  override def toString = body.map(_.toString).mkString(sep)
  override def toHTML: String = {
    val sb = new StringBuilder
    var mathBuf = new StringBuilder  // accumulates MathML content

    def flushMath(): Unit = {
      if (mathBuf.nonEmpty) {
        sb.append(s"""<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">${mathBuf.toString()}</math>""")
        mathBuf = new StringBuilder
      }
    }

    body.foreach {
      case t: SText =>
        t.mathContent match {
          case Some(mc) =>
            // Math node — accumulate into current math block
            if (mathBuf.nonEmpty && sep.nonEmpty) mathBuf.append(sep)
            mathBuf.append(mc)
          case None =>
            // Non-math node — flush accumulated math first, then emit text
            flushMath()
            sb.append(t.toHTML)
        }
      case other =>
        flushMath()
        sb.append(other.toHTML)
    }
    flushMath()
    sb.toString()
  }
  def +(rest: SSnippet): SSnippet = copy(body = this.body ++ rest.body)
}

// SPlainText may contain $X$ from §X§ in the x"" interpolator (single identifiers).
// It may also contain LaTeX commands like \mathtt{up}, \to, \gamma from String args.
// We convert both to proper MathML.
case class SPlainText(body: String) extends SText {
  override def toString = body

  // If the entire body is a math segment (from §X§), expose as mathContent
  // so SSnippet can merge it into the surrounding math block.
  override def mathContent: Option[String] = {
    val t = body.trim
    if (t.startsWith("$") && t.endsWith("$") && t.length > 2) {
      val inner = t.substring(1, t.length - 1)
      Some(latexToMathML(inner))
    } else None
  }

  override def toHTML: String = {
    // Split on $...$ boundaries (from §§ interpolation)
    val parts = body.split("\\$", -1)
    if (parts.length == 1) {
      // No $ signs — plain text, possibly with LaTeX commands
      textToHTML(body)
    } else {
      val sb = new StringBuilder
      // Collect consecutive math segments to merge into one <math> block
      var mathBuf = new StringBuilder

      def flushMath(): Unit = {
        if (mathBuf.nonEmpty) {
          sb.append(s"""<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">${mathBuf.toString()}</math>""")
          mathBuf = new StringBuilder
        }
      }

      parts.zipWithIndex.foreach { case (part, i) =>
        if (i % 2 == 0) {
          // Plain text segment
          if (part.nonEmpty) {
            flushMath()
            sb.append(textToHTML(part))
          }
        } else {
          // Math segment from $...$
          mathBuf.append(latexToMathML(part))
        }
      }
      flushMath()
      sb.toString()
    }
  }

  // Convert a plain text string (no $ delimiters) to HTML.
  // Detects embedded LaTeX commands and wraps them in <math> blocks.
  private def textToHTML(s: String): String = {
    // Check if whole string is a LaTeX command that should be math
    s.trim match {
      case t if t.startsWith("\\mathtt{") && t.endsWith("}") =>
        val inner = t.stripPrefix("\\mathtt{").stripSuffix("}")
        s"""<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline"><mi mathvariant='monospace'>$inner</mi></math>"""
      case t if t.startsWith("\\mathrm{") && t.endsWith("}") =>
        val inner = t.stripPrefix("\\mathrm{").stripSuffix("}")
        s"""<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline"><mi mathvariant='normal'>$inner</mi></math>"""
      case "\\to"    =>
        """<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline"><mo>&#x2192;</mo></math>"""
      case "\\gamma" =>
        """<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline"><mi>&#x3B3;</mi></math>"""
      case "\\pi"    =>
        """<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline"><mi>&#x3C0;</mi></math>"""
      case t =>
        // Plain text — just HTML-escape
        t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    }
  }

  // Convert a LaTeX math string (content of $...$) to a MathML fragment.
  // Handles the specific cases produced by the generators.
  private def latexToMathML(s: String): String = s.trim match {
    case "\\gamma"             => "<mi>&#x3B3;</mi>"
    case "\\pi"                => "<mi>&#x3C0;</mi>"
    case "\\to"                => "<mo>&#x2192;</mo>"
    case "\\times"             => "<mo>&#x00D7;</mo>"
    case "\\cdot"              => "<mo>&#x22C5;</mo>"
    case "\\infty"             => "<mi>&#x221E;</mi>"
    case s if s.startsWith("\\mathtt{") && s.endsWith("}") =>
      val inner = s.stripPrefix("\\mathtt{").stripSuffix("}")
      s"<mi mathvariant='monospace'>$inner</mi>"
    case s if s.startsWith("\\mathrm{") && s.endsWith("}") =>
      val inner = s.stripPrefix("\\mathrm{").stripSuffix("}")
      s"<mi mathvariant='normal'>$inner</mi>"
    case s if s.startsWith("\\mathbf{") && s.endsWith("}") =>
      val inner = s.stripPrefix("\\mathbf{").stripSuffix("}")
      s"<mi mathvariant='bold'>$inner</mi>"
    case s if s.matches("-?\\d+(\\.\\d+)?") => s"<mn>$s</mn>"
    case s =>
      // Single identifier or short expression — use <mi>
      s"<mi>${s.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")}</mi>"
  }
}

case class SMacroApplication(name: String, args: Seq[SText], flexary: Boolean) extends SText {
  override def toString = {
    val command = "\\" + name
    val argsX = args.map(_.toString)
    val argsS = if (flexary) argsX.mkString("{",",","}")
    else argsX.map(s => "{" + s + "}").mkString("")
    command + argsS
  }
  // SMacroApplication in HTML context: only uProb/CondProb reach here
  // (all other Expr macros go through SMath -> expr.toHTML directly)
  override def mathContent: Option[String] = Some(toMathMLContent)
  override def toHTML: String =
    s"""<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">${toMathMLContent}</math>"""

  private def toMathMLContent: String = {
    val a = args.map(_.toHTML)
    name match {
      case "uProb"    =>
        s"<mi>P</mi><mo>(</mo>${a.mkString("")}<mo>)</mo>"
      case "CondProb" =>
        s"<mi>P</mi><mo>(</mo>${a.headOption.getOrElse("")}<mo>|</mo>${a.drop(1).mkString("")}<mo>)</mo>"
      case _ =>
        s"<mi>\\${name}</mi>${a.map(x => s"<mo>(</mo>$x<mo>)</mo>").mkString("")}"
    }
  }
}

class SMacro(name: String) {
  def apply(args: SText*) = SMacroApplication(name, args.toList, false)
}

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
          case t: STeXSyntax => t                // already rendered — use directly
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