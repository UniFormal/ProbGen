package info.kwarc.probgen

// for dom access
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html

// for exporting scala functions to Javascript
import scala.scalajs.js.annotation.JSExportTopLevel

object main {

  private case class Slot(title: String, problem: Option[Problem[?]],
                          subs: List[Problem[?]#Subproblem], fragment: SFragment)

  private var slots: Vector[Slot] = Vector.empty

  /** the last grading result of each question, by uid */
  private val results = scala.collection.mutable.Map[String, CheckResult]()

  /** for free-text questions: the rubric criteria a student ticked, by question uid */
  private val selfChecks = scala.collection.mutable.Map[String, Set[Int]]()

  /** the sheet shown on the page; Export .tex exports this, so both always match */
  def currentSheet: Option[SDocument] =
    if (slots.isEmpty) None else Some(SDocument(slots.map(_.fragment).toList))

  /** gets called when index.html is opened */
  def main(args: Array[String]): Unit = {
    document.getElementById("new-btn")
      .addEventListener("click", (_: dom.Event) => generateAndRender())
    generateAndRender()
  }

  def generateAndRender(): Unit = {
    slots = Vector.empty
    results.clear()
    selfChecks.clear()
    val container = document.getElementById("container").asInstanceOf[html.Element]
    container.innerHTML = "<p class='loading'>Generating problems…</p>"

    // use requestAnimationFrame and setTimeout so it actually renders "Generating problems..."
    dom.window.requestAnimationFrame { _ =>
      dom.window.setTimeout(() => {
        try {
          slots = Problems.registry.map { case (title, make) => buildSlot(title, make) }.toVector
          container.innerHTML = slots.zipWithIndex.map { case (s, i) => renderCard(s, i) }.mkString
          renderSidebar()
          TeXExport.refreshIfOpen()
        } catch {
          case e: Throwable =>
            container.innerHTML = s"<pre style='color:red;padding:20px'>ERROR: ${e.getMessage}\n${e.getClass.getName}</pre>"
            e.printStackTrace()
        }
      }, 0)
    }
  }

  /** generates one problem and chooses its questions; errors stay inside the card */
  private def buildSlot(title: String, make: () => Problem[?]): Slot =
    try {
      val p = make()
      val subs = p.chooseSubproblems()
      Slot(title, Some(p), subs, SFragment(title, List(p.toSTeX(subs))))
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        val msg = SText(s"<pre class='gen-error'>${Html.esc(e.toString)}</pre>")
        Slot(title, None, Nil, SFragment(title, List(SProblem(msg, Nil))))
    }

  private def renderCard(s: Slot, i: Int): String =
    s"""<div class="problem-card" id="card-$i">
         <h2>${Html.esc(s.title)}</h2>
         <div class="content">${s.fragment.toHTML}</div>
       </div>"""

  @JSExportTopLevel("regenerateProblem")
  def regenerateProblem(i: Int): Unit = {
    if (!slots.isDefinedAt(i)) return
    slots(i).subs.foreach { s => results.remove(s.uid); selfChecks.remove(s.uid) }
    val (title, make) = Problems.registry(i)
    slots = slots.updated(i, buildSlot(title, make))
    replaceElement(s"card-$i", renderCard(slots(i), i))
    renderSidebar()
    TeXExport.refreshIfOpen()
  }

  private def findQuestion(uid: String): Option[Problem[?]#Subproblem] =
    slots.iterator.flatMap(_.subs.find(_.uid == uid)).nextOption()

  private def replaceElement(id: String, html: String): Unit = {
    val el = document.getElementById(id)
    if (el != null) el.outerHTML = html
  }

  // Sidebar

  private def earned(s: Slot): Double =
    s.subs.flatMap(sub => results.get(sub.uid).map(r => sub.awardedPoints(r))).sum

  private def total(s: Slot): Int = s.subs.map(_.pts).sum

  /** draws the sidebar from `slots` and `results`; called after every change */
  private def renderSidebar(): Unit = {
    val sidebar = document.getElementById("sidebar").asInstanceOf[html.Element]
    if (sidebar == null) return
    val problems = slots.zipWithIndex.map { case (s, i) =>
      s"""<li class="sb-problem">
           <div class="sb-row">
             <a href="#card-$i">${Html.esc(s.title)}</a>
             <span class="sb-score">${Scoring.points(earned(s))}/${total(s)}</span>
             <button class="sb-regen" title="New instance of this problem" onclick="regenerateProblem($i)">↻</button>
           </div>
         </li>"""
    }.mkString
    val all = Scoring.points(slots.map(earned).sum)
    sidebar.innerHTML =
      s"""<div class="sb-total">Score <b>$all</b> / ${slots.map(total).sum}</div>
          <ol class="sb-problems">$problems</ol>"""
  }

  /** called from within the javascript embedeed in the HTML generated from a Problem
    * (exported to Javascript so that the Javascript can find it)
    */
  @JSExportTopLevel("checkAnswer")
  def checkAnswer(id: String): Unit = {
    val inputEl = document.getElementById(s"ans-$id").asInstanceOf[html.Input]
    val fbEl    = document.getElementById(s"fb-$id").asInstanceOf[html.Element]

    findQuestion(id) match {
      case None =>
        showFeedback(fbEl, "error", "Problem not found — try generating new problems.")
      case Some(sub) =>
        // (see answers.scala) the result knows how to present itself, so adding a new kind of result never requires a change here [This is submoptimal, but it works for now]
        val res = sub.check(inputEl.value)
        inputEl.style.borderColor = res.borderColor
        showFeedback(fbEl, res.cssClass, res.messageHtml(sub.pts))
        // an unreadable answer is not an attempt
        if (!res.isInstanceOf[MalformedInput]) results(id) = res
        selfChecks.remove(id)
        renderSidebar()
    }

    // show animation so it is clear that the input was checked again
    showFlash(fbEl)
  }

  @JSExportTopLevel("selfGrade")
  def selfGrade(uid: String, criterion: Int, ticked: Boolean): Unit =
    findQuestion(uid) match {
      case Some(sub) if sub.rubric.isDefined =>
        val classes = sub.rubric.get.classes
        val now = selfChecks.getOrElse(uid, Set.empty)
        val checks = if (ticked) now + criterion else now - criterion
        selfChecks(uid) = checks
        val result = SelfGraded(checks.toList.map(i => classes(i).points).sum, sub.pts)
        results(uid) = result
        val total = document.getElementById(s"sg-$uid")
        if (total != null) total.innerHTML = result.messageHtml(sub.pts)
        renderSidebar()
      case _ =>
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
}
