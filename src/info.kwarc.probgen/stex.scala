package info.kwarc.probgen

/** This file declares Scala binders for stex syntax.
  *
  * Any problem/solution statements should be formulated in terms of these classes.
  * The generation of stex text is then taken care of generically.
  *
  * Importantly, the string interpolation syntax x" ... " can be used to construct stex code.
  * Within the " ", scala object O can be given as ${O} most of these can be converted into stex automatically.
  * Because Scala predefines $, we use § instead of $ as the latex math mode switch.
  */

/** mixin for objects that can be rendered as STeX 
trait STeXAble {
  def toSTeX: STeXSyntax
}

/** parent type of all stex syntax */
trait STeXSyntax extends STeXAble {
  def toSTeX = this
}*/

trait STeXAble {
  def toSTeX: STeXSyntax
}

// newly added by mostafa
trait STeXSyntax {
  def toSTeX = this

  // NEW: HTML renderer
  def toHTML: String = this match {

    case SProblem(intro, subs) =>
      s"""
 <div class="problem-block">
 <p>${intro.toHTML}</p>
        ${subs.map(_.toHTML).mkString("<br>")}
      </div>
 """

    case SSubproblem(pts, question, solution) =>
      s"""
 <div class="subproblem">
 <b>[$pts pts]</b><br>
        ${question.toHTML}
      </div>
 """

    case SSolution(_, body) =>
      "" // hide solutions for now

    case SItemize(items @ _*) =>
      "<ul>" + items.map(i => s"<li>${i.toHTML}</li>").mkString + "</ul>"

    case SText =>
      this.toString


    case _ =>
      this.toString
  }
}
//new added 2

//

/** mixin for objects that can be rendered as STeX */


/** parent type of all stex syntax
 trait STeXSyntax extends STeXAble {
 def toSTeX = this
 }*/

case class SParams(pars: (String,String)*) extends STeXSyntax {
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
}

case class SDocument(body: List[SFragment]) extends SEnvironment("document", 4) {
  def toStringFull = {
    """\documentclass{article}
      |\usepackage{stexlight}
      |""".stripMargin + toString
  }
}

object SDocument {
  def apply(t: String, p: SProblem): SDocument = SDocument(List(SFragment(t, List(p))))
}

case class SFragment(title: String, body: List[SProblem]) extends SEnvironment("sfragment", 3) {
  override def args = List(title)
}

case class SProblem(intro: STeXSyntax, subproblems: List[SSubproblem]) extends SEnvironment("sproblem", 2) {
  def body = intro::subproblems
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
  override def toString = "\\item " + body
}
case class SCenter(body: Seq[STeXSyntax]) extends SEnvironment("center")
case class STabular(cellHead: SText, columnHeads: Seq[SText], rowHeads: Seq[SText], cells: Seq[(Int,Int,SText)]) extends SEnvironment("tabular") {
  def makeRow(cs: Seq[SText]): SText = cs.head ++ cs.tail.flatMap(s => Seq(SText(" & "), s)) ++ Seq(SText("\\\\"))
  override def args = {
    val cs = Range(0,columnHeads.length).map(_ => "c").mkString("")
    List("l|"++ cs)
  }
  def body = {
    val headerRow = makeRow(cellHead +: columnHeads)
    val bodyRows = {
      rowHeads.zipWithIndex.map {case (r,i) =>
        val values = Range(0,columnHeads.length).toList
          .map(j => cells.find(c => c._1==i && c._2 == j).map(_._3).getOrElse(SText(" ")))
        makeRow(r :: values)
      }
    }
    headerRow +: SText("\\hline") +: bodyRows
  }
}

trait SText extends STeXSyntax {
  def ++(more: Seq[STeXSyntax]) = SSnippet(this+:more)
  def +(more: STeXSyntax) = if (more == null) this else SSnippet(List(this,more))
}

case class SMath(body: SText) extends SText {
  override def toString = "$" + body.toString + "$"
}

/* comma-separated sequence of math objects */
case class SMaths(body: List[SText]) extends SText {
  override def toString = body.map(b => SMath(b).toString).mkString(", ")
}

case class SSnippet(body: Seq[STeXSyntax], sep: String = "") extends SText {
  override def toString = body.mkString(sep)
  def +(rest: SSnippet) = copy(body = this.body++rest.body)
}

case class SPlainText(body: String) extends SText {
  override def toString = body
  def togglesMath = {
    var i = 0
    val len = body.length
    var inmath = false
    while (i < len) {
      val c = body(i)
      if (c == '$') {
        if (i+1<len && body(i+1) == '$') i += 1
        inmath = !inmath
      } else if (c == '\\') {
        i += 1
        if (i<len) {
          val d = body(i)
          if (d == '(' || d == '[' || d == ')' || d == ']') {
            inmath = !inmath
          }
        }
      }
      i += 1
    }
    inmath
  }
}

case class SMacroApplication(name: String, args: List[SText], flexary: Boolean) extends SText {
  override def toString = {
    val command = "\\" + name
    val argsX = args.map(_.toString)
    val argsS = if (flexary) argsX.mkString("{",",","}")
      else argsX.map(s => "{" + s + "}").mkString("")
    command + argsS
  }
}

class SMacro(name: String) {
  def apply(args: SText*) = SMacroApplication(name, args.toList, false)
}

object SText {
  implicit class STextInterpolator(sc: StringContext) {
    def m(args: Any*): SText = interpolate(true, args*)
    def x(args: Any*): SText = interpolate(false, args*)
    def interpolate(startInMathmode: Boolean, args: Any*): SText = {
      var inMath = startInMathmode
      var partsS = sc.parts.toList.map {s =>
        val sR = s.replace('§', '$').replace("\\n","\n")
        SPlainText(sR)
      }
      var snippets: List[STeXSyntax] = List(partsS.head)
      inMath = inMath ^ partsS.head.togglesMath
      partsS = partsS.tail
      val argsS = args.toList.foreach {arg =>
        val argS = arg match {
          case sx: STeXAble => sx.toSTeX
          case sx: List[_] if sx.forall(_.isInstanceOf[STeXAble]) =>
            SSnippet(sx.map(_.asInstanceOf[STeXAble].toSTeX),", ")
          case s: String => apply(s)
          case a => Expr.fromAnyO(a) match {
            /*case Some(e) => if (inMath) e.toSTeX else e.toSTeXTop*/ //remove the compiler error
            case None => SPlainText(a.toString)
          }
        }
        snippets ::= argS
        snippets ::= partsS.head
        inMath = inMath ^ partsS.head.togglesMath
        partsS = partsS.tail
      }
      SSnippet(snippets.reverse)
    }
  }

  def make(a: Any) = x"$a"
  def apply(args: STeXSyntax*): SText = SSnippet(args.toList)
  def apply(s: String): SText = SPlainText(s)
  implicit def fromInt(i: Int): SText = SPlainText(i.toString)
  def !(s: String) = SPlainText(s)
}
