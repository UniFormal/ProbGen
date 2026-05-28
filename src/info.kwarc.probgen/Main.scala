package info.kwarc.probgen

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/**
 * ScalaJS entry point — replaces Test/WebServer.
 * Generates problems in-browser, handles answer checking client-side.
 */
object Main {

  // Maps subproblem hashCode id -> raw solution string
  private val solutions = scala.collection.mutable.Map[String, String]()

  // ---------------------------------------------------------------------------
  // Entry point
  // ---------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    // Wire the "New Problems" button
    val btn = document.getElementById("new-btn")
    if (btn != null)
      btn.addEventListener("click", (_: dom.Event) => generateAndRender())

    generateAndRender()
  }

  // ---------------------------------------------------------------------------
  // Generation + rendering
  // ---------------------------------------------------------------------------

  def generateAndRender(): Unit = {
    solutions.clear()

    val container = document.getElementById("container").asInstanceOf[html.Element]
    container.innerHTML = "<p class='loading'>Generating problems…</p>"

    val sb = new StringBuilder
    sb ++= renderProblem("MDP Problem",         MDPGenerator.make())
    sb ++= renderProblem("Probability Problem", BasicProbabilityProblemGenerator.make())
    sb ++= renderProblem("Search Problem",      SearchProblemGenerator.make())

    container.innerHTML = sb.toString()

    // Ask MathJax to typeset the new content
    retypeset(container)
  }

  def renderProblem(title: String, gen: Problem[_]): String = {
    // FR: store gen in a table of problem instances; retrieve the instance and its subproblem when checking a solution
    val subs = gen.chooseSubproblems()
    val prob = gen.toSTeX(subs)
    val doc  = SDocument("problem", prob)

    // Collect solution strings keyed by subproblem id
    doc.body.foreach { frag =>
      frag.body.foreach { p =>
        p.subproblems.foreach { sub =>
          val id  = sub.hashCode.abs.toString
          val sol = sub.solution.body.map(_.toString).mkString(" ").trim
          solutions(id) = sol
        }
      }
    }

    s"""<div class="problem-card">
         <h2>$title</h2>
         <div class="content">${doc.toHTML}</div>
       </div>"""
  }

  // ---------------------------------------------------------------------------
  // Answer checking — called from HTML onclick="checkAnswer('id')"
  // ---------------------------------------------------------------------------

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
      showFeedback(fbEl, "correct", "&#10003; Correct! Well done.")
    } else {
      inputEl.style.borderColor = "#e05c5c"
      val solEl = document.getElementById(s"sol-$id")
      val expHtml = if (solEl != null) solEl.asInstanceOf[html.Element].innerHTML else escHtml(expected)
      showFeedback(fbEl, "wrong", s"&#10007; Not quite. Expected: <strong>$expHtml</strong>")
    }

    retypeset(fbEl)
  }

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  def showFeedback(el: html.Element, cls: String, htmlStr: String): Unit = {
    val h = el
    h.className    = s"feedback $cls"
    h.innerHTML    = htmlStr
    h.style.display = "block"
  }

  def escHtml(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  def normalise(s: String): String = {
    val cleaned = s.trim.toLowerCase
      .replaceAll("\\s+", "")
      .replace("*", "")
      .replace("&middot;", "").replace("·", "").replace("×", "")
      .replace("&lt;", "<").replace("&gt;", ">")
      .replace("&le;", "<=").replace("&ge;", ">=")
      .replace("&ne;", "!=")
      .replace("\\(", "").replace("\\)", "")
      .replace("{", "").replace("}", "")
    // Sort additive terms so a+d == d+a
    cleaned.split("\\+").map(_.trim).sorted.mkString("+")
  }

  def retypeset(node: dom.Element): Unit = {
    val mj = js.Dynamic.global.MathJax
    if (!js.isUndefined(mj) && !js.isUndefined(mj.typesetPromise))
      mj.typesetPromise(js.Array(node))
  }
}