package info.kwarc.probgen

/** common parent of all renderable syntax */
trait STeXSyntax {
  def toHTML: String
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
}

abstract class SList(n: String, items: List[SText]) extends SEnvironment(n) {
  def body = items.map(SItem(_))
}
case class SItemize(items: SText*) extends SList("itemize", items.toList) {
  override def toHTML: String =
    "<ul>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("") + "</ul>"
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

case class STabular(cellHead: SText, columnHeads: Seq[SText],rowHeads: Seq[SText],cells: Seq[(Int, Int, SText)])
  extends SEnvironment("tabular") {
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

trait SText extends STeXSyntax {
  def ++(more: Seq[STeXSyntax]): SText = SSnippet(this +: more)
  def +(more: STeXSyntax): SText = if (more == null) this else SSnippet(List(this, more))
}

case class SMath(expr: Expr) extends SText {
  override def toString = "$" + expr.toSTeX + "$"
  override def toHTML: String = s"<math>${expr.toHTML}</math>"
}

case class SSnippet(body: Seq[STeXSyntax], sep: String = "") extends SText {
  override def toString = body.mkString(sep)
  def toHTML = body.map(_.toHTML).mkString(sep)
  def +(rest: SSnippet) = copy(body = this.body++rest.body)
}

case class SPlainText(body: String) extends SText {
  override def toString = body
  def toHTML = body
}

/** @param singleArgsNum == i ---> 0: {a_1}...{a_i}{a_i+1, ..., a_n}; i=-1 is like i=n*/
case class SMacroApplication(name: String, args: Seq[SText], singleArgsNum: Int) extends SText {
  override def toString = {
    val argsX = args.map(_.toString)
    val (singleArgs,seqArgs) = if (singleArgsNum == -1) (argsX,Nil) else argsX.splitAt(singleArgsNum)
    val singleArgsS = if (singleArgs.isEmpty) "" else singleArgs.map(s => s"{$s}").mkString("")
    val seqArgsS = if (seqArgs.isEmpty) "" else seqArgsX.mkString("{", ",", "}")
    s"\\$name$singleArgsS$seqArgsS"
  }
  def toHTML = "" // only allowed for invisible content
}

case class SMacro(name: String) {
  def apply(args: SText*) = SMacroApplication(name, args.toList, false)
}

/** String interpolation */
object SText {
  implicit class STextInterpolator(sc: StringContext) {
    def x(args: Any*): SText = {
      var partsS = sc.parts.toList.map { s =>
        SPlainText(s.replace('§', '$').replace("\\n", "\n"))
      }
      var snippets: List[STeXSyntax] = List(partsS.head)
      partsS = partsS.tail
      args.toList.foreach {arg =>
        val argS: STeXSyntax = arg match {
          case t: STeXSyntax => t
          case s: String     => SPlainText(s)
          case e: Expr       => SMath(e)
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