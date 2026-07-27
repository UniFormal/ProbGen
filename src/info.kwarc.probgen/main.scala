package info.kwarc.probgen

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.timers.setTimeout


object main {

  private val subproblemMap = scala.collection.mutable.Map[String, Problem[?]#Subproblem]()

  def main(args: Array[String]): Unit = {
    document.getElementById("new-btn")
      .addEventListener("click", (_: dom.Event) => generateAndRender())
    generateAndRender()
  }

  def generateAndRender(): Unit = {
    subproblemMap.clear()
    val container = document.getElementById("container").asInstanceOf[html.Element]
    container.innerHTML = "<p class='loading'>Generating problems…</p>"

    // use setTimeout so it already renders "Generating problems..." 
    setTimeout(1) {
        try {
          val sb = new StringBuilder
          sb ++= renderProblem("Probability Problem", BasicProbabilityProblemGenerator.make())
          sb ++= renderProblem("MDP Problem",         MDPGenerator.make())
          sb ++= renderProblem("Search Problem",      SearchProblemGenerator.make())
          container.innerHTML = sb.toString()
        } catch {
          case e: Throwable =>
            container.innerHTML =
              s"<pre style='color:red;padding:20px'>ERROR: ${e.getMessage}\n${e.getClass.getName}</pre>"
            e.printStackTrace()
        }
      }
  }

  def renderProblem(title: String, gen: Problem[?]): String = {
    // Choose subproblems first — these are Problem#Subproblem objects with checkSolution
    val subs = gen.chooseSubproblems()

    // Store each subproblem by its id BEFORE rendering, using the same hashCode
    // that SSubproblem.toHTML uses for its id attribute
    // Use the Problem#Subproblem's own hashCode as the id.
    // Subproblem.toSTeX() passes this same id into SSubproblem so HTML ids match.
    subs.foreach { sub =>
      val id = sub.hashCode.abs.toString
      subproblemMap(id) = sub
    }

    val prob = gen.toSTeX(subs)
    val doc  = SDocument("problem", prob)

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
      showFlash(fbEl)
      return
    }

    subproblemMap.get(id) match {
      case None =>
        showFeedback(fbEl, "error", "Problem not found — try generating new problems.")
      case Some(sub) =>
        sub.checkSolution(userAns) match {
          case Correct() =>
            inputEl.style.borderColor = "#52c97a"
            showFeedback(fbEl, "correct", "&#10003; Correct!")
          case Incorrect(hint) =>
            inputEl.style.borderColor = "#e05c5c"
            showFeedback(fbEl, "wrong", s"&#10007; $hint")
          case NotCheckable(expected) =>
            inputEl.style.borderColor = "#e05c5c"
            showFeedback(fbEl, "wrong",
              s"&#10007; Not quite. Expected: <strong>${escHtml(expected)}</strong>")
        }
    }

    // show animation so it is clear that the input was checked again
    showFlash(fbEl)
  }

  def showFlash(el: html.Element): Unit = {
    val flashClass = "flash"
    el.classList.remove(flashClass)
    dom.window.setTimeout(() => el.classList.add(flashClass), 0)
    dom.window.setTimeout(() => el.classList.remove(flashClass), 250)
  }

  def showFeedback(el: html.Element, cls: String, msg: String): Unit = {
    el.className     = s"feedback $cls"
    el.innerHTML     = msg
    el.style.display = "block"
  }

  def escHtml(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
}
