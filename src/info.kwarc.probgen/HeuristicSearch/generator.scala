package info.kwarc.probgen

object HeuristicSearchProblemGenerator extends ProblemGenerator[HeuristicSearchProblem] {
  def make(): HeuristicSearchProblem = {
    var problem = candidate()
    while (!problem.graph.hasThreePathsToGoal) problem = candidate()
    val edges = problem.graph.edges.map { edge =>
      val cost = Generator.chooseWeighted((1 to 8).map(value => (value, (9 - value).toDouble)))
      edge.copy(cost = Some(cost))
    }
    val weighted = problem.graph.copy(edges = edges)
    val minimumCosts = weighted.minimumCostsToGoal
    val nodes = weighted.nodes.map { node =>
      val heuristic = minimumCosts(node.name) match {
        case Some(0) => 0
        case Some(bound) => Generator.chooseInt(1, bound)
        // Any finite estimate is admissible for a dead end (true cost is infinite).
        case None => Generator.chooseInt(1, 8)
      }
      node.copy(heuristic = Some(heuristic))
    }
    problem.copy(graph = weighted.copy(nodes = nodes))
  }

  private def candidate(): HeuristicSearchProblem = {
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
    val nodes = names.map(HeuristicSearchNode(_)).toList
    val graph = HeuristicSearchGraph(nodes, edges, names.head)
    HeuristicSearchProblem(graph, positions)
  }
}
