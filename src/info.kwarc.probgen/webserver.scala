package info.kwarc.probgen

import info.kwarc.probgen.MDPGenerator
import info.kwarc.probgen.BasicProbabilityProblemGenerator
import info.kwarc.probgen.SearchProblemGenerator

import com.sun.net.httpserver.{HttpServer, HttpExchange}
import java.net.InetSocketAddress
import java.io.OutputStream
import java.nio.charset.StandardCharsets

object WebServer {

  // ------------------------------------------------------------------ //
  //  Problem generation                                                  //
  // ------------------------------------------------------------------ //

  // Keep solutions in memory so /check can look them up by subproblem id.
  private var currentSolutions: Map[String, String] = Map.empty

  def generateProblems(): String = {
    val sb = new StringBuilder
    currentSolutions = Map.empty

    def addProblem(title: String, doc: SDocument): Unit = {
      doc.body.foreach { frag =>
        frag.body.foreach { prob =>
          prob.subproblems.foreach { sub =>
            val id  = sub.hashCode.abs.toString
            val sol = sub.solution.body.map(_.toString).mkString(" ").trim
            currentSolutions += (id -> sol)
          }
        }
      }
      val html = doc.toHTML
      sb.append(s"""
        <div class="problem">
          <h2>$title</h2>
          <div class="content">$html</div>
        </div>
      """)
    }

    val mdp = MDPGenerator.make()
    addProblem("MDP Problem", SDocument("MDP Problem", mdp.toSTeX(mdp.chooseSubproblems())))

    val prob = BasicProbabilityProblemGenerator.make()
    addProblem("Probability Problem", SDocument("Probability Problem", prob.toSTeX(prob.chooseSubproblems())))

    val search = SearchProblemGenerator.make()
    addProblem("Search Problem", SDocument("Search Problem", search.toSTeX(search.chooseSubproblems())))

    sb.toString()
  }

  // ------------------------------------------------------------------ //
  //  Normalise answer strings for lenient comparison                    //
  // ------------------------------------------------------------------ //

  def normalise(s: String): String =
    s.trim
      .toLowerCase
      .replaceAll("\\s+", "")
      .replace("*", "").replace("x", "")
      .replace("&middot;", "").replace("·", "").replace("×", "")
      .replace("&lt;", "<").replace("&gt;", ">")
      .replace("&le;", "<=").replace("&ge;", ">=")
      .replace("&ne;", "!=")
      .replace("\\(", "").replace("\\)", "")

  // ------------------------------------------------------------------ //
  //  POST /check                                                         //
  // ------------------------------------------------------------------ //

  def handleCheck(exchange: HttpExchange): Unit = {
    val body   = new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
    val params = body.split("&").map { kv =>
      val p = kv.split("=", 2)
      if (p.length == 2) p(0) -> java.net.URLDecoder.decode(p(1), "UTF-8")
      else p(0) -> ""
    }.toMap

    val id         = params.getOrElse("id", "")
    val userAnswer = params.getOrElse("answer", "")
    val expected   = currentSolutions.getOrElse(id, "")

    val correct = normalise(userAnswer) == normalise(expected)

    val feedbackText =
      if (correct) "Correct! Well done."
      else s"Not quite. The expected answer is: ${escape(expected)}"

    val json =
      s"""{"correct":$correct,"feedback":"${escape(feedbackText)}","expected":"${escape(expected)}"}"""

    val bytes = json.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json; charset=UTF-8")
    exchange.sendResponseHeaders(200, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  }

  private def escape(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", " ")

  // ------------------------------------------------------------------ //
  //  Page HTML                                                           //
  // ------------------------------------------------------------------ //

  def pageHtml(content: String): String = s"""<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>ProbGen</title>
<script>
window.MathJax = {
  tex: { inlineMath: [['$$', '$$'], ['\\\\(', '\\\\)']] }
};
</script>
<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
<style>
*, *::before, *::after { box-sizing: border-box; }
body { font-family: Arial, sans-serif; margin: 40px; background: #f0f2f5; color: #222; }
h1   { margin-bottom: 24px; }
.container { display: flex; flex-direction: column; gap: 20px; }
.problem {
  background: white; border-radius: 10px; padding: 24px;
  box-shadow: 0 1px 4px rgba(0,0,0,.12);
}
.problem h2 { margin: 0 0 16px; font-size: 1.2rem; color: #1a1a2e; }
.problem-block p { margin: 0 0 12px; }
.subproblem {
  background: #f7f9fc; border-left: 4px solid #2f80ed;
  border-radius: 0 8px 8px 0; padding: 14px 16px; margin: 12px 0;
}
.subproblem b { color: #2f80ed; }
.answer-row { display: flex; gap: 8px; margin-top: 10px; align-items: center; }
.answer-row input {
  flex: 1; padding: 8px 12px; border: 1.5px solid #ccd3de;
  border-radius: 6px; font-size: 15px; outline: none; transition: border-color .15s;
}
.answer-row input:focus { border-color: #2f80ed; }
.answer-row button {
  padding: 8px 18px; background: #2f80ed; color: white; border: none;
  border-radius: 6px; font-size: 15px; cursor: pointer; transition: background .15s;
}
.answer-row button:hover  { background: #1c60b3; }
.answer-row button:disabled { background: #aac4f0; cursor: default; }
.feedback {
  margin-top: 8px; padding: 8px 12px; border-radius: 6px;
  font-size: 14px; display: none;
}
.feedback.correct { background: #e6f9ec; border: 1px solid #52c97a; color: #1a7a3a; }
.feedback.wrong   { background: #fff0f0; border: 1px solid #e05c5c; color: #8b1a1a; }
.feedback.error   { background: #fff8e6; border: 1px solid #f0a800; color: #7a5000; }
.gen-btn {
  display: inline-block; padding: 12px 24px; background: #2f80ed; color: white;
  text-decoration: none; border-radius: 8px; font-size: 16px;
  margin-bottom: 16px; transition: background .15s;
}
.gen-btn:hover { background: #1c60b3; }
table { border-collapse: collapse; margin: 15px 0; }
th, td { border: 1px solid #aaa; padding: 6px 12px; text-align: center; }
th { background: #eef0f4; }
</style>
</head>
<body>
<h1>&#127891; ProbGen: Practice Problems</h1>
<a class="gen-btn" href="/">&#8635; New Problems</a>
<div class="container">
$content
</div>
<script>
async function checkAnswer(id) {
  const input = document.getElementById('ans-' + id);
  const fb    = document.getElementById('fb-'  + id);
  const answer = input.value.trim();
  if (!answer) { showFb(fb, 'error', 'Please enter an answer first.'); return; }

  const btn = input.nextElementSibling;
  btn.disabled = true;
  btn.textContent = 'Checking...';

  try {
    const res  = await fetch('/check', {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: 'id=' + encodeURIComponent(id) + '&answer=' + encodeURIComponent(answer)
    });
    const data = await res.json();
    if (data.correct) {
      input.style.borderColor = '#52c97a';
      showFb(fb, 'correct', '&#10003; Correct! Well done.');
    } else {
      input.style.borderColor = '#e05c5c';
      const exp = data.expected ? ' Expected: <strong>' + esc(data.expected) + '</strong>' : '';
      showFb(fb, 'wrong', '&#10007; Not quite.' + exp);
    }
    if (window.MathJax) MathJax.typesetPromise([fb]);
  } catch(e) {
    showFb(fb, 'error', 'Server unreachable.');
  }

  btn.disabled = false;
  btn.textContent = 'Check';
}

function showFb(el, cls, html) {
  el.className = 'feedback ' + cls;
  el.innerHTML = html;
  el.style.display = 'block';
}

function esc(s) {
  return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

document.addEventListener('keydown', e => {
  if (e.key === 'Enter' && e.target.matches('.answer-row input'))
    e.target.nextElementSibling.click();
});
</script>
</body>
</html>"""

  // ------------------------------------------------------------------ //
  //  Server bootstrap                                                    //
  // ------------------------------------------------------------------ //

  def main(args: Array[String]): Unit = {
    val server = HttpServer.create(new InetSocketAddress(8080), 0)

    server.createContext("/", exchange => {
      if (exchange.getRequestMethod == "GET") {
        val html  = pageHtml(generateProblems())
        val bytes = html.getBytes(StandardCharsets.UTF_8)
        exchange.getResponseHeaders.add("Content-Type", "text/html; charset=UTF-8")
        exchange.sendResponseHeaders(200, bytes.length)
        val os: OutputStream = exchange.getResponseBody
        os.write(bytes); os.close()
      } else {
        exchange.sendResponseHeaders(405, -1)
        exchange.getResponseBody.close()
      }
    })

    server.createContext("/check", exchange => {
      if (exchange.getRequestMethod == "POST") handleCheck(exchange)
      else { exchange.sendResponseHeaders(405, -1); exchange.getResponseBody.close() }
    })

    server.start()
    println("Server running at http://localhost:8080")
  }
}
