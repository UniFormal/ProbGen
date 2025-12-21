package info.kwarc.probgen

/** mixin for objects that can be rendered as STeX */
trait STeXAble {
  def toSTeX: STeXSyntax
}

/** parent type of all stex syntax */
trait STeXSyntax extends STeXAble {
  def toSTeX = this
}

case class SParams(pars: (String,String)*) extends STeXSyntax {
  override def toString = {
    if (pars.isEmpty) "" else
      pars.map {case (key,value) => s"$key={$value}"}.mkString("[", ", ", "]")
  }
}

abstract class SEnvironment(name: String) extends STeXSyntax {
  def args: List[String] = Nil
  def params: SParams = SParams()
  def body: List[STeXSyntax]
  override def toString = {
    val argsS = args.map(a => s"{$a}").mkString("")
    s"\\begin{$name}$argsS${params}\n${body.mkString("\n")}\n\\end{$name}"
  }
}

case class SDocument(body: List[SFragment]) extends SEnvironment("document")

case class SFragment(title: String, body: List[SProblem]) extends SEnvironment("sfragment") {
  override def params = SParams("title" -> title)
}

case class SProblem(intro: STeXSyntax, subproblems: List[SSubproblem]) extends SEnvironment("sproblem") {
  def body = intro::subproblems
}

case class SSubproblem(pts: Int, question: SText, solution: SSolution) extends SEnvironment("subproblem") {
  override def params = SParams("pts" -> pts.toString)
  def body = List(question, solution)
}

case class SSolution(testspace: Float, body: List[SText]) extends SEnvironment("solution") {
  override def params = SParams("testspace" -> (testspace.toString + "cm"))
}

abstract class SList(n: String, val body: List[SItem]) extends SEnvironment(n)
case class SItemize(items: SItem*) extends SList("itemize", items.toList)
case class SEnumerate(items: SItem*) extends SList("enumerate", items.toList)
case class SItem(body: SText) extends STeXSyntax {
  override def toString = "\\item " + body
}

trait SText extends STeXSyntax

case class SMath(body: SText) extends SText {
  override def toString = "$" + body.toString.replace("$","") + "$"
}

/* comma-separated sequence of math objects */
case class SMaths(body: List[SText]) extends SText {
  override def toString = body.map(b => SMath(b).toString).mkString(", ")
}

case class SSnippet(body: List[STeXSyntax], sep: String = "") extends SText {
  override def toString = body.mkString(sep)
}

case class SPlainText(body: String) extends SText {
  override def toString = body
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
    def x(args: Any*): SText = {
      val partsS = sc.parts.toList.map {s =>
        val sR = s.replace('§', '$').replace("\\n","\n")
        SPlainText(sR)
      }
      val argsS = args.toList.map {
        case sx: STeXAble => sx.toSTeX
        case sx: List[_] if sx.forall(_.isInstanceOf[STeXAble]) =>
          SSnippet(sx.map(_.asInstanceOf[STeXAble].toSTeX), ", ")
        case s: String => apply(s)
        case a => Expr.fromAnyO(a) match {
          case Some(e) => e.toSTeXTop
        }
      }
      val pairs = argsS.zip(partsS.tail)
      val snippets = pairs.flatMap {case (s,p) => List(s,p)}
      SSnippet(partsS.head :: snippets)
    }
  }

  def apply(args: STeXSyntax*): SText = SSnippet(args.toList)
  def apply(s: String): SText = SPlainText(s)
  implicit def fromInt(i: Int) = SPlainText(i.toString)
  def !(s: String) = SPlainText(s)
}
