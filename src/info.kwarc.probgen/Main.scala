package info.kwarc.probgen

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

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
        container.innerHTML = s"<pre style='color:red;padding:20px'>ERROR: ${e.getMessage}\n${e.getClass.getName}</pre>"
        println("ProbGen error: " + e.getMessage)
        e.printStackTrace()
    }
  }

  def renderProblem(title: String, gen: Problem[?]): String = {
    val subs = gen.chooseSubproblems()
    val prob = gen.toSTeX(subs)
    val doc  = SDocument("problem", prob)

    // Collect solutions keyed by subproblem id
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

  @JSExportTopLevel("checkAnswer")
  def checkAnswer(id: String): Unit = {
    val inputEl  = document.getElementById(s"ans-$id").asInstanceOf[html.Input]
    val fbEl     = document.getElementById(s"fb-$id").asInstanceOf[html.Element]
    val userAns  = inputEl.value.trim

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
      val solEl  = document.getElementById(s"sol-$id")
      val expHtml = if (solEl != null) solEl.asInstanceOf[html.Element].innerHTML
      else escHtml(expected)
      showFeedback(fbEl, "wrong", s"&#10007; Not quite. Expected: <strong>$expHtml</strong>")
    }
  }

  def showFeedback(el: html.Element, cls: String, msg: String): Unit = {
    el.className   = s"feedback $cls"
    el.innerHTML   = msg
    el.style.display = "block"
  }

  def escHtml(s: String): String =
    s.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")

  def normalise(s: String): String =
    s.trim.toLowerCase
      .replaceAll("\\s+", "")
      .replace("{","").replace("}","")
      .split("\\+").map(_.trim).sorted.mkString("+")
}