package info.kwarc.probgen

/** Layout and naming are computed together so names follow visual reading order. */
case class HeuristicSearchLayout(
    graph: HeuristicSearchGraph,
    positions: Map[String, (Double, Double)]
)

object HeuristicSearchLayout {
  def apply(input: HeuristicSearchGraph): HeuristicSearchLayout = {
    val names = input.nodes.map(_.name)
    require(names.nonEmpty && names.size <= 26, "A graph must contain between 1 and 26 nodes")
    require(names.distinct.size == names.size, "Node names must be unique")
    require(names.contains(input.root), "The root must exist")
    require(input.edges.forall(e => names.contains(e.from) && names.contains(e.to)),
      "Edges must refer to existing nodes")
    require(input.edges.map(e => (e.from, e.to)).distinct.size == input.edges.size,
      "Edges must be unique")

    val parents = names.map(n => n -> input.edges.filter(_.to == n).map(_.from)).toMap
    val children = names.map(n => n -> input.edges.filter(_.from == n).map(_.to)).toMap
    require(parents(input.root).isEmpty, "The root must have no predecessors")

    // Kahn's algorithm both detects cycles and assigns longest-path ranks.
    var remaining = names.toSet
    var ranks = Map.empty[String, Int]
    while (remaining.nonEmpty) {
      val ready = remaining.toList.filter(n => parents(n).forall(ranks.contains)).sorted
      require(ready.nonEmpty, "The graph must be acyclic")
      ready.foreach { n =>
        require(n == input.root || parents(n).nonEmpty,
          "Every node must be reachable from the root")
        ranks += n -> parents(n).map(p => ranks(p) + 1).maxOption.getOrElse(0)
      }
      remaining --= ready
    }

    // Prefer the last input name if it is a sink, otherwise choose the last sink.
    val terminal = names.sorted.reverse.find(n => children(n).isEmpty).get
    // Keep a bounded set of partial layouts. A row can contain a horizontal
    // edge only between neighbors, so it never runs through another node.
    val beamWidth = if (names.size <= 12) 32 else 6
    val rowWidth = math.max(3, ranks.values.groupBy(identity).values.map(_.size).max)
    val edges = input.edges.sortBy(e => (e.from, e.to))
    var candidates = Vector(Vector(Vector(input.root)))
    for (_ <- 1 until names.size) {
      candidates = candidates.flatMap { rows =>
        val placed = rows.flatten.toSet
        val lastRow = rows.last
        val ready = names.sorted.filter { n =>
          !placed(n) && parents(n).forall(placed) &&
            (n != terminal || placed.size == names.size - 1)
        }
        ready.flatMap { n =>
          val nextRow = rows :+ Vector(n)
          val canShareRow = rows.size > 1 && lastRow.size < rowWidth &&
            parents(n).filter(lastRow.contains).forall(_ == lastRow.last)
          if (canShareRow) Vector(rows.updated(rows.size - 1, lastRow :+ n), nextRow)
          else Vector(nextRow)
        }
      }.map(rows => (layoutCost(rows, edges), rows)).sortBy(_._1).take(beamWidth).map(_._2)
    }
    val rows = candidates.head
    val renamed = rows.flatten.zipWithIndex.map { case (n, i) =>
      n -> ('A' + i).toChar.toString
    }.toMap
    val width = rows.map(_.size).max - 1.0
    val positions = rows.zipWithIndex.flatMap { case (row, rank) =>
      val offset = (width - row.size + 1) / 2
      row.zipWithIndex.map { case (n, column) =>
        renamed(n) -> ((offset + column) * 1.4, rank * 1.3)
      }
    }.toMap
    val byName = input.nodes.map(n => n.name -> n).toMap
    val graph = HeuristicSearchGraph(
      rows.flatten.toList.map(n => byName(n).copy(name = renamed(n))),
      input.edges.map(e => e.copy(from = renamed(e.from), to = renamed(e.to))),
      renamed(input.root)
    )
    new HeuristicSearchLayout(graph, positions)
  }

  private def layoutCost(rows: Vector[Vector[String]], edges: List[HeuristicSearchEdge]): Double = {
    val positions = rows.zipWithIndex.flatMap { case (row, rank) =>
      row.zipWithIndex.map { case (name, column) =>
        name -> ((2 * column - row.size + 1, 2 * rank))
      }
    }.toMap
    val segments = edges.flatMap { edge =>
      for {
        from <- positions.get(edge.from)
        to <- positions.get(edge.to)
      } yield (from, to)
    }.toVector

    def side(a: (Int, Int), b: (Int, Int), c: (Int, Int)): Int =
      (b._1 - a._1) * (c._2 - a._2) - (b._2 - a._2) * (c._1 - a._1)

    var crossings = 0
    segments.indices.foreach { i =>
      val (a, b) = segments(i)
      for (j <- 0 until i) {
        val (c, d) = segments(j)
        if (side(a, b, c) * side(a, b, d) < 0 && side(c, d, a) * side(c, d, b) < 0)
          crossings += 1
      }
    }
    val length = segments.map { case ((x1, y1), (x2, y2)) =>
      math.hypot(x2 - x1, y2 - y1)
    }.sum
    3 * rows.size + 5 * crossings + 0.2 * length
  }

}
