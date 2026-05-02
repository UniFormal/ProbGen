package info.kwarc.probgen

// Generators
import info.kwarc.probgen.MDPGenerator
import info.kwarc.probgen.BasicProbabilityProblemGenerator
import info.kwarc.probgen.SearchProblemGenerator

import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress
import java.io.OutputStream

object WebServer {

  
  // GENERATE 3 PROBLEMS 
  
  def generateProblems(): String = {

    val sb = new StringBuilder

    // ----------- MDP -----------
    val mdp = MDPGenerator.make()
    val mdpHtml = mdp.toSTeX(mdp.chooseSubproblems()).toHTML

    sb.append(
      s"""
      <div class="problem">
        <h2>MDP Problem</h2>
        <div class="content">$mdpHtml</div>
      </div>
      """
    )

    // ----------- PROBABILITY -----------
    val prob = BasicProbabilityProblemGenerator.make()
    val probHtml = prob.toSTeX(prob.chooseSubproblems()).toHTML

    sb.append(
      s"""
      <div class="problem">
        <h2>Probability Problem</h2>
        <div class="content">$probHtml</div>
      </div>
      """
    )

    // ----------- SEARCH -----------
    val search = SearchProblemGenerator.make()
    val searchHtml = search.toSTeX(search.chooseSubproblems()).toHTML

    sb.append(
      s"""
      <div class="problem">
        <h2>Search Problem</h2>
        <div class="content">$searchHtml</div>
      </div>
      """
    )

    sb.toString()
  }

  
  // MAIN SERVER
  
  def main(args: Array[String]): Unit = {

    val server = HttpServer.create(new InetSocketAddress(8080), 0)

    server.createContext("/", exchange => {

      val content = generateProblems()

      val html =
        s"""
<html>
<head>
<meta charset="UTF-8">
<title>ProbGen</title>

<script>
window.MathJax = {
  tex: {
    inlineMath: [['$$', '$$'], ['\\(', '\\)']]
  }
};
</script>
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