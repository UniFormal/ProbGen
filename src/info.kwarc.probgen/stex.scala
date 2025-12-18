package info.kwarc.probgen

trait STeXSyntax {
  def toSTeX: String
}

case class SParams(pars: (String,String)*) extends STeXSyntax {
  def toSTeX = {
    if (pars.isEmpty) "" else
      pars.map {case (key,value) => s"$key={$value}"}.mkString("[", ", ", "]")
  }
}

abstract class SEnvironment(name: String) extends STeXSyntax {
  def args: List[String] = Nil
  def params: SParams = SParams()
  def body: List[STeXSyntax]
  def toSTeX = {
    val argsS = args.map(a => s"{$a}").mkString("")
    s"\\begin{$name}$argsS${params.toSTeX}\n${body.map(_.toSTeX).mkString("\n")}\n\\end{$name}"
  }
}

case class SDocument(body: List[SFragment]) extends SEnvironment("document")

case class SFragment(title: String, body: List[SProblem]) extends SEnvironment("sfragment") {
  override def params = SParams("title" -> title)
}

case class SProblem(intro: STeXSyntax, subproblems: List[SSubproblem]) extends SEnvironment("sproblem") {
  def body = intro::subproblems
}

case class SSubproblem(pts: Int, question: SText, solution: SSolution) extends SEnvironment("solution") {
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
  def toSTeX = "\\item " + body.toSTeX + "\\n"
}

trait SText extends STeXSyntax

case class SMath(body: SText) extends SText {
  def toSTeX = "$" + body.toSTeX + "$"
}

case class SSnippet(body: List[STeXSyntax]) extends SText {
  override def toSTeX = body.map(_.toSTeX).mkString("")
}

case class SPlainText(body: String) extends SText {
  def toSTeX = body
}

case class SMacroApplication(name: String, args: List[SText], flexary: Boolean) extends SText {
  override def toSTeX = {
    val command = "\\" + name
    val argsX = args.map(_.toSTeX)
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
        val sR = s.replace('§', '$')
        SPlainText(sR)
      }
      val argsS = args.map {
        case e: Expr => e.toSTeX
        case sx: STeXSyntax => sx
      }
      SSnippet(partsS.head :: partsS.tail.zip(argsS).flatMap {case (p,s) => List(p,s)})
    }
  }

  def apply(args: STeXSyntax*) = SSnippet(args.toList)
  implicit def fromInt(i: Int) = SPlainText(i.toString)
  def !(s: String) = SPlainText(s)
}
