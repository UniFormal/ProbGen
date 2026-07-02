package info.kwarc.probgen

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

  // Maps subproblem id -> plain text of the expected answer
  private val solutions = scala.collection.mutable.Map[String, String]()

  def main(args: Array[String]): Unit = {
    document.getElementById("new-btn")
      .addEventListener("click", (_: dom.Event) => generateAndRender())
    generateAndRender()
  }

  def generateAndRender(): Unit = {
    solutions.clear()
    val container = document.getElementById("container").asInstanceOf[html.Element]
    container.innerHTML = "<p class='loading'>Generating problems…</p>"
    try {
      val sb = new StringBuilder
      sb ++= renderProblem("MDP Problem",         MDPGenerator.make())
      sb ++= renderProblem("Probability Problem", BasicProbabilityProblemGenerator.make())
      sb ++= renderProblem("Search Problem",      SearchProblemGenerator.make())
      container.innerHTML = sb.toString()
    } catch {
      case e: Throwable =>
        container.innerHTML =
          s"<pre style='color:red;padding:20px'>ERROR: ${e.getMessage}\n${e.getClass.getName}</pre>"
        e.printStackTrace()
    }
  }

  def renderProblem(title: String, gen: Problem[?]): String = {
    val subs = gen.chooseSubproblems()
    val prob = gen.toSTeX(subs)
    val doc  = SDocument("problem", prob)

    // Store solution as plain text via toText — no HTML tags, no LaTeX macros.
    // This is exactly what a user would type when they see the rendered answer.
    doc.body.foreach { frag =>
      frag.body.foreach { p =>
        p.subproblems.foreach { sub =>
          val id  = sub.hashCode.abs.toString
          val sol = sub.solution.body.map(_.toText).mkString(" ").trim
          solutions(id) = sol
        }
      }
    }

    s"""<div class="problem-card">
         <h2>$title</h2>
         <div class="content">${doc.toHTML}</div>
       </div>"""
  }

  @JSExportTopLevel("checkAnswer")
  def checkAnswer(id: String): Unit = {
    val inputEl = document.getElementById(s"ans-$id").asInstanceOf[html.Input]
    val fbEl    = document.getElementById(s"fb-$id").asInstanceOf[html.Element]
    val userAns = inputEl.value.trim

    if (userAns.isEmpty) {
      showFeedback(fbEl, "error", "Please enter an answer first.")
      return
    }

    val expected = solutions.getOrElse(id, "")
    val correct  = normalise(userAns) == normalise(expected)

    if (correct) {
      inputEl.style.borderColor = "#52c97a"
      showFeedback(fbEl, "correct", "&#10003; Correct!")
    } else {
      inputEl.style.borderColor = "#e05c5c"
      // Show the expected plain text in the feedback
      showFeedback(fbEl, "wrong",
        s"&#10007; Not quite. Expected: <strong>${escHtml(expected)}</strong>")
    }
  }

  def showFeedback(el: html.Element, cls: String, msg: String): Unit = {
    el.className     = s"feedback $cls"
    el.innerHTML     = msg
    el.style.display = "block"
  }

  def escHtml(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  // Normalise two plain-text answers for lenient comparison.
  // Handles: case, whitespace, and additive term ordering (a+d == d+a).
  def normalise(s: String): String = {
    val lower = s.toLowerCase.trim
    val noSpace = lower.split("\\s+").mkString("")
    // Sort additive terms so "a + d + e" == "e + d + a"
    noSpace.split("\\+").map(_.trim).filter(_.nonEmpty).sorted.mkString("+")
  }
}