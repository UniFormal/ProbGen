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
    val lastRank = ranks.values.max
    ranks += terminal -> lastRank
    var rows = (0 to lastRank).map(rank => names.filter(ranks(_) == rank).sorted).toVector

    // Barycenter sweeps reduce crossings while keeping the root and terminal fixed.
    for (_ <- 0 until 4) {
      for (rank <- 1 to lastRank) {
        val order = rows.flatten.zipWithIndex.toMap
        rows = rows.updated(rank, rows(rank).sortBy { n =>
          val adjacent = parents(n)
          (adjacent.map(order(_).toDouble).sum / adjacent.size, n)
        })
      }
      for (rank <- (1 until lastRank).reverse) {
        val order = rows.flatten.zipWithIndex.toMap
        rows = rows.updated(rank, rows(rank).sortBy { n =>
          val adjacent = children(n)
          (if (adjacent.isEmpty) order(n).toDouble
           else adjacent.map(order(_).toDouble).sum / adjacent.size, n)
        })
      }
    }
    rows = rows.updated(lastRank, rows(lastRank).filterNot(_ == terminal) :+ terminal)
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
}
