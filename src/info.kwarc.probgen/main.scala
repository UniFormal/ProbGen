package info.kwarc.probgen

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExportTopLevel

object main {

  private val subproblemMap = scala.collection.mutable.Map[String, Problem[?]#Subproblem]()

  def main(args: Array[String]): Unit = {
    val p = CSPGenerator.make()
    //val p = SearchProblemGenerator.make()
    // val p = BasicProbabilityProblemGenerator.make()
    //val p = MDPGenerator.make()
    val subs = p.chooseSubproblems()
    val stex = p.toSTeX(subs)
    if (args.nonEmpty && args(0) == "--full") {
      val d = SDocument("problem", stex)
      println(d.toStringFull)
    } else {
      println(stex)
    }
  }

  def showFeedback(el: html.Element, cls: String, msg: String): Unit = {
    el.className     = s"feedback $cls"
    el.innerHTML     = msg
    el.style.display = "block"
  }

  def escHtml(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
}