package info.kwarc.probgen

/**
 * This file declares Scala binders for stex syntax.
 *
 * Any problem/solution statements should be formulated in terms of these classes.
 * The generation of stex text is then taken care of generically.
 *
 * Importantly, the string interpolation syntax x" ... " can be used to construct stex code.
 * Within the " ", scala object O can be given as ${O} most of these can be converted into stex automatically.
 * Because Scala predefines $, we use § instead of $ as the latex math mode switch.
 */

/**
 * parent type of all stex syntax
 */
trait STeXSyntax {

  def toHTML: String = this match {

    // ---------------- PROBLEM ----------------
    case SProblem(intro, subs) =>
      s"""
      <div class="problem-block">
        <p>${intro.toHTML}</p>
        ${subs.map(_.toHTML).mkString("\n")}
      </div>
      """

    // ---------------- SUBPROBLEM ----------------
    case SSubproblem(pts, question, _) =>
      s"""
      <div class="subproblem">
        <b>[$pts pts]</b><br>
        ${question.toHTML}
      </div>
      """

    // ---------------- SOLUTION ----------------
    case SSolution(_, _) =>
      ""

    // ---------------- ITEMIZE ----------------
    case SItemize(items @ _*) =>
      "<ul>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("\n") + "</ul>"

    // ---------------- ENUMERATE ----------------
    case SEnumerate(items @ _*) =>
      "<ol>" + items.map(i => s"<li>${i.toHTML}</li>").mkString("\n") + "</ol>"

    // ---------------- ITEM ----------------
    case SItem(body) =>
      s"<li>${body.toHTML}</li>"

    // ---------------- CENTER ----------------
    case SCenter(body) =>
      "<div style='text-align:center'>" +
        body.map(_.toHTML).mkString("<br>") +
        "</div>"

    // ---------------- TABULAR ----------------
    case t: STabular =>
      s"<pre>${t.toString}</pre>"

    // ---------------- ENVIRONMENT SAFETY ----------------
    case env: SEnvironment =>
      env.body.map(_.toHTML).mkString("\n")

    // ---------------- MATH ----------------
    case t: SText =>
      t.toString

    case other =>
      other.toString
  }
}

/*****************/

case class SParams(pars: (String, String)*) {
  override def toString =
    if (pars.isEmpty) ""
    else pars.map { case (k, v) => s"$k={$v}" }.mkString("[", ", ", "]")
}

/**
 * Base environment
 */
abstract class SEnvironment(name: String, level: Int = 0) extends STeXSyntax {
  def args: List[String] = Nil
  def params: SParams = SParams()
  def body: Seq[STeXSyntax]

  // IMPORTANT: keep structure internally only
  override def toString = body.map(_.toString).mkString("\n")
}

case class SDocument(body: List[SFragment]) extends SEnvironment("document", 4) {
  def toStringFull =
    """\documentclass{article}
      |\usepackage{stexlight}
      |""".stripMargin + toString
}

object SDocument {
  def apply(t: String, p: SProblem): SDocument =
    SDocument(List(SFragment(t, List(p))))
}

case class SFragment(title: String, body: List[SProblem])
  extends SEnvironment("sfragment", 3) {

  override def args = List(title)
}

case class SProblem(intro: STeXSyntax, subproblems: List[SSubproblem])
  extends SEnvironment("sproblem", 2) {

  def body = intro :: subproblems
}

case class SSubproblem(pts: Int, question: SText, solution: SSolution)
  extends SEnvironment("subproblem", 1) {

  override def params = SParams("pts" -> pts.toString)
  def body = List(question, solution)
}

case class SSolution(testspace: Float, body: List[SText])
  extends SEnvironment("solution") {

  override def params =
    SParams("testspace" -> (testspace.toString + "cm"))
}

/**
 * Lists
 */
abstract class SList(n: String, items: List[SText]) extends SEnvironment(n) {
  def body = items.map(SItem(_))
}

case class SItemize(items: SText*) extends SList("itemize", items.toList)
case class SEnumerate(items: SText*) extends SList("enumerate", items.toList)

/**
 * Item (FIXED: no LaTeX leakage)
 */
case class SItem(body: SText) extends STeXSyntax {
  override def toString = body.toString
}

case class SCenter(body: Seq[STeXSyntax]) extends SEnvironment("center")

/**
 * Tabular
 */
case class STabular(
                     cellHead: SText,
                     columnHeads: Seq[SText],
                     rowHeads: Seq[SText],
                     cells: Seq[(Int, Int, SText)]
                   ) extends SEnvironment("tabular") {

  def makeRow(cs: Seq[SText]): SText =
    SSnippet(cs.flatMap(c => Seq(c, SText(" & "))).dropRight(1) :+ SText("\\\\"))

  override def args =
    List("l|" + ("c" * columnHeads.length))

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
}

/**
 * TEXT BASE
 */
trait SText extends STeXSyntax {
  def ++(more: Seq[STeXSyntax]) = SSnippet(this +: more)
  def +(more: STeXSyntax) = SSnippet(List(this, more))
}

case class SMath(expr: Expr) extends SText {
  override def toString = "$" + expr.toSTeX + "$"
}

case class SSnippet(body: Seq[STeXSyntax], sep: String = "") extends SText {
  override def toString = body.map(_.toString).mkString(sep)
}

case class SPlainText(body: String) extends SText {
  override def toString = body
}

/**
 * MACROS (SAFE RENDERING)
 */
case class SMacroApplication(name: String, args: Seq[SText], flexary: Boolean)
  extends SText {

  override def toString: String = {

    val a = args.map(_.toString)

    name match {

      case "tup"      => "(" + a.mkString(", ") + ")"
      case "set"      => "\\{ " + a.mkString(", ") + " \\}"
      case "apply"    => a.headOption.map(h => h + "(" + a.drop(1).mkString(", ") + ")").getOrElse("")
      case "equals"   => a.mkString(" = ")
      case "intplus"  => a.mkString(" + ")
      case "intminus" => a.mkString(" - ")
      case "inttimes" => a.mkString(" · ")
      case "intdiv"   => a.mkString(" / ")
      case "intlethan" | "intlessthan" =>
        a.mkString(" < ")
      case "intgreaterthan" =>
        a.mkString(" > ")

      case _ =>
        "\\" + name + a.map("{" + _ + "}").mkString
    }
  }
}

/**
 * INTERPOLATION
 */
object SText {

  implicit class STextInterpolator(sc: StringContext) {

    def x(args: Any*): SText = {

      var partsS = sc.parts.toList.map(s =>
        SPlainText(s.replace('§', '$').replace("\\n", "\n"))
      )

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