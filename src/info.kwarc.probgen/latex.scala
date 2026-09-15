package info.kwarc.probgen

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** the problems on one sheet, used by both the page and the .tex export */
object Problems {
  def all(): List[(String, Problem[?])] = List(
    "MDP Problem"         -> MDPGenerator.make(),
    "Probability Problem" -> BasicProbabilityProblemGenerator.make(),
    "Search Problem"      -> SearchProblemGenerator.make(),
    "Adversarial Search"  -> MinimaxProblemGenerator.make(),
    "Logic Problem"       -> LogicProblemGenerator.make(),
    "CSP Problem"         -> CSPGenerator.make()
  )
}

object TeXExport {

  /** the sheet currently shown on the page, ready to hand to pdflatex */
  def sheet(): String =
    main.currentSheet.map(_.toStringFull).getOrElse("% no problems have been generated yet")

  /** the sTeX source as a string; callable from the browser console */
  @JSExportTopLevel("problemTeX")
  def problemTeX(): String = sheet()

  /** shows the sTeX in the page and offers it as a problem.tex download */
  @JSExportTopLevel("exportTeX")
  def exportTeX(): Unit = {
    val panel = document.getElementById("output-panel").asInstanceOf[html.Element]
    if (panel == null) return
    val tex = try sheet()
              catch { case e: Throwable => "% generation failed: " + e.getMessage }

    panel.innerHTML =
      s"""<div class="panel-section">
         |  <h3>Generated sTeX</h3>
         |  <div class="panel-box">
         |    <div class="panel-title">problem.tex
         |      <span class="panel-note">&mdash; save it next to stexlight.sty, then run
         |      <code>pdflatex problem.tex</code></span></div>
         |    <div class="panel-row">
         |      <button id="tex-dl" onclick="downloadTeX()">&#11015; Download problem.tex</button>
         |    </div>
         |    <textarea id="tex-out" readonly rows="24"
         |      style="width:100%;margin-top:10px;font-family:'Courier New',monospace;
         |             font-size:12px;padding:10px;border:1.5px solid #ccd3de;border-radius:7px;"
         |      >${Html.esc(tex)}</textarea>
         |  </div>
         |</div>""".stripMargin
    panel.style.display = "block"
    panel.scrollIntoView()
  }

  /** saves the currently shown sTeX as problem.tex */
  @JSExportTopLevel("downloadTeX")
  def downloadTeX(): Unit = {
    val area = document.getElementById("tex-out").asInstanceOf[html.TextArea]
    val tex  = if (area != null) area.value else sheet()
    val blob = new dom.Blob(js.Array[dom.BlobPart](tex))
    val url  = dom.URL.createObjectURL(blob)
    val a    = document.createElement("a").asInstanceOf[html.Anchor]
    a.href = url
    a.setAttribute("download", "problem.tex")
    a.style.display = "none"
    document.body.appendChild(a)
    a.click()
    document.body.removeChild(a)
    dom.URL.revokeObjectURL(url)
  }
}
