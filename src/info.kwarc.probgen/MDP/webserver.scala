package info.kwarc.probgen.MDP

import info.kwarc.probgen.MDP.MDPGenerator
import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress
import java.io.{File, PrintWriter, OutputStream}
import java.nio.file.Files
import sys.process._

object WebServer {

  var currentPdf = ""

  def generateProblem(): String = {

    val timestamp = System.currentTimeMillis()

    val texFile = s"problem_$timestamp.tex"
    val pdfFile = s"problem_$timestamp.pdf"

    val writer = new PrintWriter(new File(texFile))

    writer.println(
      """
\documentclass{article}
\usepackage{amsmath}
\begin{document}

\section*{AI Practice Problems}
"""
    )

    val numProblems = 5

    for(i <- 1 to numProblems){

      val p = MDPGenerator.make()
      val subs = p.chooseSubproblems()

      writer.println(s"\\subsection*{Problem $i}")
      writer.println(p.toSTeX(subs))
      writer.println("\\vspace{1cm}")
      writer.println("\\hrule")
      writer.println("\\vspace{1cm}")
    }

    writer.println(
      """
\end{document}
"""
    )

    writer.close()

    val cmd = s"pdflatex -interaction=nonstopmode $texFile"
    cmd.!

    currentPdf = pdfFile
    pdfFile
  }

  def main(args: Array[String]): Unit = {

    val server = HttpServer.create(new InetSocketAddress(8080),0)

    server.createContext("/", exchange => {

      val html =
        s"""
<html>

<head>
<title>AI Problem Generator</title>

<style>

body{
font-family:Arial;
margin:40px;
background:#f5f5f5;
}

.container{
background:white;
padding:20px;
border-radius:10px;
box-shadow:0 0 10px #ccc;
}

button{
padding:12px 20px;
font-size:16px;
border:none;
background:#2f80ed;
color:white;
border-radius:6px;
cursor:pointer;
margin-right:10px;
}

button:hover{
background:#1c60b3;
}

iframe{
width:100%;
height:700px;
border:none;
margin-top:20px;
}

</style>

</head>

<body>

<h1>ProbGen: Practice Problems</h1>

<div class="container">

<a href="/generate">
<button>Generate New Problem</button>
</a>

<br><br>

<a href="/pdf">
<button>Download PDF</button>
</a>

<iframe src="/pdf"></iframe>

</div>

</body>
</html>
"""

      val bytes = html.getBytes("UTF-8")

      exchange.sendResponseHeaders(200, bytes.length)

      val os = exchange.getResponseBody
      os.write(bytes)
      os.close()
    })

    server.createContext("/generate", exchange => {

      val pdf = generateProblem()

      exchange.getResponseHeaders.add("Location", "/")
      exchange.sendResponseHeaders(302,-1)
      exchange.close()
    })

    server.createContext("/pdf", exchange => {

      val file = new File(currentPdf)

      if(file.exists()){

        val bytes = Files.readAllBytes(file.toPath)

        exchange.getResponseHeaders.add("Content-Type","application/pdf")

        exchange.sendResponseHeaders(200, bytes.length)

        val os: OutputStream = exchange.getResponseBody
        os.write(bytes)
        os.close()

      } else {

        val msg = "Generate a problem first."
        val bytes = msg.getBytes("UTF-8")

        exchange.sendResponseHeaders(200, bytes.length)

        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }

    })

    server.start()

    println("Server running at http://localhost:8080")
  }
}