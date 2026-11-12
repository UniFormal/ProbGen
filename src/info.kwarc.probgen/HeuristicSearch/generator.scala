package info.kwarc.probgen

object HeuristicSearchProblemGenerator extends ProblemGenerator[HeuristicSearchProblem] {
  def make(): HeuristicSearchProblem = {
    val widths = 1 :: List.fill(3)(Generator.chooseInt(2, 3))
    val boundaries = widths.scanLeft(0)(_ + _)
    val names = (0 until boundaries.last).map(i => ('A' + i).toChar.toString)
    val rows = boundaries.zip(boundaries.tail).map { case (start, end) => names.slice(start, end) }
    val positions = rows.zipWithIndex.flatMap { case (row, rank) =>
      val offset = (widths.max - row.size) / 2.0
      row.zipWithIndex.map { case (name, column) =>
        name -> ((offset + column) * 1.4, rank * 1.3)
      }
    }.toMap

    val connections = rows.zip(rows.tail).map { case (parents, children) =>
      // Sorting the chosen parents preserves their order across the two rows.
      val chosen = children.map(_ => Generator.choose(parents)).sorted
      val required = chosen.zip(children).map { case (from, to) => HeuristicSearchEdge(from, to) }
      val available = for {
        from <- parents
        to <- children
        edge = HeuristicSearchEdge(from, to)
        if !required.contains(edge)
        if !required.exists(other =>
          (from < other.from && to > other.to) || (from > other.from && to < other.to))
      } yield edge
      (required, available)
    }
    val possibleSideways = rows.flatMap { row =>
      row.zip(row.tail).map { case (from, to) => HeuristicSearchEdge(from, to) }
    }
    val sideways = Generator.chooseSome(possibleSideways, 0, possibleSideways.size, allowRep = false)
    val available = connections.flatMap(_._2)
    val extra = if (available.nonEmpty && Generator.chooseBoolean(0.5)) List(Generator.choose(available))
                else Nil
    val edges = connections.flatMap(_._1) ++ sideways ++ extra
    val graph = HeuristicSearchGraph(names.map(HeuristicSearchNode(_)).toList, edges, names.head)
    HeuristicSearchProblem(graph, positions)
  }
}
