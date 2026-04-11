package info.kwarc.probgen

import info.kwarc.probgen.MDPGenerator
import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress
import java.io.OutputStream

object WebServer {

  // Extract points if needed (safe)
  def extractPoints(s: String): String = {
    val r = "\\[pts=\\{(\\d+)\\}\\]".r
    r.findFirstMatchIn(s).map(_.group(1)).getOrElse("")
  }

  // FINAL SAFE CLEANER (MathJax friendly)
  def clean(s: String): String = {
    s
      // remove environments
      .replaceAll("\\\\begin\\{.*?\\}", "")
      .replaceAll("\\\\end\\{.*?\\}", "")

      // remove grading / UI noise
      .replaceAll("\\[pts=\\{[^}]*\\}\\]", "")
      .replaceAll("\\[testspace=\\{[^}]*\\}\\]", "")
      .replaceAll("%%%%%%%%+", "")
      .replaceAll("Unknown environment 'itemize'", "")
      .replaceAll("\\[\\]", "")

      // FIX CORE STeX MACROS → LaTeX SAFE
      .replaceAll("\\\\tup\\{(.*?)\\}", "($1)")
      .replaceAll("\\\\mathtt\\{(.*?)\\}", "\\\\mathtt{$1}")
      .replaceAll("\\\\set\\{(.*?)\\}", "\\\\{ $1 \\\\}")

      // item lists
      .replaceAll("\\\\item", "•")

      // normalize spacing only
      .replaceAll("\n{2,}", "<br><br>")
      .replaceAll("\n", "<br>")
  }

  def generateProblem(): String = {

    val sb = new StringBuilder
    val numProblems = 5

    for (i <- 1 to numProblems) {

      val p = MDPGenerator.make()
      val subs = p.chooseSubproblems()

      val raw = p.toSTeX(subs).toString
      val cleaned = clean(raw)

      sb.append(
        s"""
        <div class="problem">
          <h2>Problem $i</h2>
          <div class="content">
            $cleaned
          </div>
        </div>
        """
      )
    }

    sb.toString()
  }

  def main(args: Array[String]): Unit = {

    val server = HttpServer.create(new InetSocketAddress(8080), 0)

    server.createContext("/", exchange => {

      val content = generateProblem()

      val html =
        s"""
<html>
<head>
<meta charset="UTF-8">
<title>ProbGen</title>

<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>

<style>

body {
  font-family: Arial;
  margin: 40px;
  background: #f5f5f5;
}

.container {
  background: white;
  padding: 20px;
  border-radius: 10px;
}

.problem {
  background: #eef2f7;
  padding: 15px;
  margin-bottom: 20px;
  border-radius: 8px;
}

.content {
  line-height: 1.6;
  font-size: 15px;
}

button {
  padding: 12px 20px;
  font-size: 16px;
  border: none;
  background: #2f80ed;
  color: white;
  border-radius: 6px;
  cursor: pointer;
}

button:hover {
  background: #1c60b3;
}

</style>

</head>

<body>

<h1>ProbGen: Practice Problems</h1>

<a href="/"><button>Generate New Problems</button></a>

<br><br>

<div class="container">
$content
</div>

</body>
</html>
"""

      val bytes = html.getBytes("UTF-8")

      exchange.getResponseHeaders.add("Content-Type", "text/html; charset=UTF-8")
      exchange.sendResponseHeaders(200, bytes.length)

      val os: OutputStream = exchange.getResponseBody
      os.write(bytes)
      os.close()
    })

    server.start()

    println("Server running at http://localhost:8080")
  }
}