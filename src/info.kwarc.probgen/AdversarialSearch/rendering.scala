package info.kwarc.probgen

/** the "αβ-pruning" of the lecture
  *
  * The framework has no LaTeX command for α and β, so we render them directly.
  * The "-pruning" is part of this node so that the text after it starts with a
  * space and [[SSnippet]] does not insert one before the hyphen.
  */
case object SAlphaBetaPruning extends SText {
  override def toString = "$\\alpha\\beta$-pruning"
  def toHTML = """<span class="math"><i>α</i><i>β</i></span>-pruning"""
  override def toText = "alpha-beta pruning"
}

/** draws a game tree, as an SVG picture in the browser and as a tikz picture in
  * sTeX
  *
  * Both pictures are built from the same layout, so the exam sheet and the
  * browser show the same tree. Leaves are drawn with their value below the
  * node, except for the leaves in `hidden`, which are drawn empty because the
  * student is supposed to reason about their value.
  *
  * The layout puts every leaf in its own column from left to right and every
  * inner node above the middle of its children, which is how the trees in the
  * exams look.
  */
case class SGameTree(tree: GameTree, hidden: Set[String] = Set()) extends SText {

  // ── Layout ───────────────────────────────────────────────────────────────
  // Positions are in abstract units: x counts leaf columns, y counts levels.

  private lazy val placed: List[(GameTree, Double, Int)] = {
    var nextLeafX = 0.0
    def go(t: GameTree, d: Int): (Double, List[(GameTree, Double, Int)]) =
      t.children match {
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
    placed.map(p => p._1.name -> (p._2, p._3)).toMap

  private lazy val edges: List[(String, String)] =
    placed.flatMap(p => p._1.children.map(c => (p._1.name, c.name)))

  private lazy val maxX = placed.map(_._2).max
  private lazy val maxDepth = placed.map(_._3).max

  /** the value drawn below a node, if any */
  private def labelBelow(t: GameTree): Option[Int] = t match {
    case GameLeaf(n, v) if !hidden.contains(n) => Some(v)
    case _                                     => None
  }

  // ── Browser: an SVG picture ──────────────────────────────────────────────

  private val unitX = 56.0 // px per leaf column
  private val unitY = 80.0 // px per level
  private val radius = 15.0
  private val pad = 20.0
  private val ink = "#1a1a2e"

  private def cx(x: Double) = pad + radius + x * unitX
  private def cy(d: Int) = pad + radius + d * unitY

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
    val circles = placed.map { case (_, x, d) =>
      s"""<circle cx="${cx(x)}" cy="${cy(d)}" r="$radius" fill="#fff" stroke="$ink" stroke-width="1.4"/>"""
    }
    val names = placed.map { case (n, x, d) =>
      s"""<text x="${cx(x)}" y="${cy(d)}" text-anchor="middle" dominant-baseline="central" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="15" fill="$ink">${n.name}</text>"""
    }
    val labels = placed.flatMap { case (n, x, d) =>
      labelBelow(n).map { v =>
        s"""<text x="${cx(x)}" y="${cy(d) + radius + 17}" text-anchor="middle" font-family="Georgia, 'Times New Roman', serif" font-size="14" fill="$ink">$v</text>"""
      }
    }
    s"""<svg viewBox="0 0 $w $h" width="$w" height="$h" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;height:auto;display:block;margin:14px auto">${(lines ::: circles ::: names ::: labels).mkString("")}</svg>"""
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
      s"\\node[gn$label] (${n.name}) at (${cm(x * tikzX)},${cm(-d * tikzY)}) {$$${n.name}$$};"
    }
    val draws = edges.map { case (f, t) => s"\\draw ($f) -- ($t);" }
    ("\\begin{center}" ::
      "\\begin{tikzpicture}[gn/.style={draw,circle,minimum size=7mm,inner sep=1pt}]" ::
      nodes ::: draws :::
      List("\\end{tikzpicture}", "\\end{center}")).mkString("\n")
  }
}
