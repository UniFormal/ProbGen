package info.kwarc.probgen

/** Shared tree layout and SVG/TikZ rendering. Labels and costs belong to callers. */
case class STreeDiagram[N](
    tree: N,
    name: N => String,
    children: N => List[N],
    labelBelow: N => Option[Int],
    heuristic: N => Option[Int],
    cost: (String, String) => Option[Int],
    directed: Boolean = false
) extends SText {

  // ── Layout ───────────────────────────────────────────────────────────────
  // Positions are in abstract units: x counts leaf columns, y counts levels.

  private lazy val placed: List[(N, Double, Int)] = {
    var nextLeafX = 0.0
    def go(t: N, d: Int): (Double, List[(N, Double, Int)]) =
      children(t) match {
        case Nil =>
          val x = nextLeafX
          nextLeafX += 1
          (x, List((t, x, d)))
        case cs =>
          val below = cs.map(c => go(c, d + 1))
          val xs = below.map(_._1)
          val x = (xs.min + xs.max) / 2
          (x, (t, x, d) :: below.flatMap(_._2))
      }
    go(tree, 0)._2
  }

  private lazy val posOf: Map[String, (Double, Int)] =
    placed.map(p => name(p._1) -> (p._2, p._3)).toMap

  private lazy val edges: List[(String, String)] =
    placed.flatMap(p => children(p._1).map(c => (name(p._1), name(c))))

  private lazy val maxX = placed.map(_._2).max
  private lazy val maxDepth = placed.map(_._3).max

  // ── Browser: an SVG picture ──────────────────────────────────────────────

  private val unitX = if (directed) 100.0 else 56.0 // px per leaf column
  private val unitY = if (directed) 100.0 else 80.0 // px per level
  private val radius = if (directed) 28.0 else 15.0
  private val pad = 20.0
  private val ink = "#1a1a2e"

  private def cx(x: Double) = pad + radius + x * unitX
  private def cy(d: Int) = pad + radius + d * unitY

  private def nodeLabel(n: N): String =
    name(n) + heuristic(n).map(v => s": $v").getOrElse("")

  def toHTML: String = {
    val w = 2 * (pad + radius) + maxX * unitX
    val h = 2 * (pad + radius) + maxDepth * unitY + 12 // room for the values

    // Lines are drawn first and from centre to centre, the filled circles then
    // cover their ends.
    val lines = edges.map { case (f, t) =>
      val (fx, fd) = posOf(f)
      val (tx, td) = posOf(t)
      s"""<line x1="${cx(fx)}" y1="${cy(fd)}" x2="${cx(tx)}" y2="${cy(td)}" stroke="$ink" stroke-width="1.1"/>"""
    }
    val edgeLabels = edges.flatMap { case (f, t) =>
      val (fx, fd) = posOf(f)
      val (tx, td) = posOf(t)
      cost(f, t).map { v =>
        s"""<text x="${(cx(fx) + cx(tx)) / 2 + 10}" y="${(cy(fd) + cy(td)) / 2}" text-anchor="middle" font-family="Georgia, serif" font-size="14" fill="$ink" stroke="white" stroke-width="4" paint-order="stroke">$v</text>"""
      }
    }
    val arrows = if (!directed) Nil else edges.map { case (f, t) =>
      val (fx, fd) = posOf(f)
      val (tx, td) = posOf(t)
      val dx = cx(tx) - cx(fx)
      val dy = cy(td) - cy(fd)
      val length = math.hypot(dx, dy)
      val ux = dx / length
      val uy = dy / length
      val x = cx(tx) - ux * (radius + 1)
      val y = cy(td) - uy * (radius + 1)
      s"""<polygon points="$x,$y ${x - 8 * ux + 4 * uy},${y - 8 * uy - 4 * ux} ${x - 8 * ux - 4 * uy},${y - 8 * uy + 4 * ux}" fill="$ink"/>"""
    }
    val circles = placed.map { case (_, x, d) =>
      s"""<circle cx="${cx(x)}" cy="${cy(d)}" r="$radius" fill="#fff" stroke="$ink" stroke-width="1.4"/>"""
    }
    val names = placed.map { case (n, x, d) =>
      s"""<text x="${cx(x)}" y="${cy(d)}" text-anchor="middle" dominant-baseline="central" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="15" fill="$ink">${Html.esc(nodeLabel(n))}</text>"""
    }
    val labels = placed.flatMap { case (n, x, d) =>
      labelBelow(n).map { v =>
        s"""<text x="${cx(x)}" y="${cy(d) + radius + 17}" text-anchor="middle" font-family="Georgia, 'Times New Roman', serif" font-size="14" fill="$ink">$v</text>"""
      }
    }
    s"""<svg viewBox="0 0 $w $h" width="$w" height="$h" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;height:auto;display:block;margin:14px auto">${(lines ::: arrows ::: edgeLabels ::: circles ::: names ::: labels).mkString("")}</svg>"""
  }

  // ── Exam sheet: a tikz picture ───────────────────────────────────────────
  // stexlight.sty already loads tikz, so this needs no extra package.

  // cm per leaf column, narrowed for wide trees so that the picture stays
  // inside the text width of the exam sheet
  private lazy val tikzX = math.min(1.15, 11.5 / math.max(maxX, 1.0))
  private val tikzY = 1.5 // cm per level

  private def cm(d: Double) = (math.round(d * 100) / 100.0).toString

  override def toString = {
    val nodes = placed.map { case (n, x, d) =>
      val label = labelBelow(n).map(v => s",label=below:{$$$v$$}").getOrElse("")
      s"\\node[gn$label] (${name(n)}) at (${cm(x * tikzX)},${cm(-d * tikzY)}) {$$${nodeLabel(n)}$$};"
    }
    val draws = edges.map { case (f, t) =>
      val arrow = if (directed) "[->]" else ""
      val label = cost(f, t).map(v => s" node[midway,fill=white,inner sep=2pt] {$$$v$$}").getOrElse("")
      s"\\draw$arrow ($f) --$label ($t);"
    }
    ("\\begin{center}" ::
      "\\begin{tikzpicture}[gn/.style={draw,circle,minimum size=7mm,inner sep=1pt}]" ::
      nodes ::: draws :::
      List("\\end{tikzpicture}", "\\end{center}")).mkString("\n")
  }
}
