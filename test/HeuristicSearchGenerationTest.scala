package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

class HeuristicSearchGenerationTest extends AnyFunSuite {
  test("generated graphs are varied, reachable, and fit their rows without edge crossings") {
    val problems = List.fill(60)(HeuristicSearchProblemGenerator.make())
    assert(problems.map(p => (p.positions, p.graph.edges)).distinct.size > 1)
    val horizontalCounts = problems.map { problem =>
      val actual = problem.graph.edges.count(e => problem.positions(e.from)._2 == problem.positions(e.to)._2)
      val possible = problem.positions.size - problem.positions.values.map(_._2).toSet.size
      (actual, possible)
    }
    assert(horizontalCounts.exists { case (actual, possible) => actual == possible && possible > 3 })
    assert(horizontalCounts.exists { case (actual, possible) => actual < possible })
    for (problem <- problems) {
      val graph = problem.graph
      val positions = problem.positions
      val rows = positions.toList.groupBy(_._2._2).toList.sortBy(_._1)
        .map { case (_, row) => row.sortBy(_._2._1).map(_._1) }
      val names = graph.nodes.map(_.name)
      assert((7 to 10).contains(names.size))
      assert(rows.size == 4 && rows.head == List("A"))
      assert(graph.root == "A")
      assert(rows.flatten == names)
      assert(names == names.indices.map(i => ('A' + i).toChar.toString).toList)
      assert(!graph.edges.exists(_.from == rows.last.last))
      assert(graph.edges.distinct == graph.edges)
      assert(graph.nodes.forall(_.heuristic.isEmpty))
      assert(graph.edges.forall(_.cost.isEmpty))

      val rowOf = rows.zipWithIndex.flatMap { case (row, index) => row.map(_ -> index) }.toMap
      val horizontal = graph.edges.filter(e => rowOf(e.from) == rowOf(e.to))
      assert(horizontal.size <= rows.map(_.size - 1).sum)
      assert(horizontal.forall { edge =>
        val row = rows(rowOf(edge.from))
        row.indexOf(edge.to) == row.indexOf(edge.from) + 1
      })

      val downward = graph.edges.filterNot(horizontal.contains)
      assert(downward.forall(e => rowOf(e.to) == rowOf(e.from) + 1))
      assert(downward.map(_.to).toSet == names.tail.toSet)
      assert((0 to 1).contains(downward.size - names.size + 1))
      assert(downward.groupBy(e => rowOf(e.from)).values.forall { band =>
        band.forall(a => !band.exists(b => a.from < b.from && a.to > b.to))
      })
    }
  }

  test("SVG and TikZ render the generated nodes and arrows without numeric labels") {
    val problem = HeuristicSearchProblemGenerator.make()
    val diagram = SHeuristicSearchGraph(problem.graph, problem.positions)
    assert("<text ".r.findAllIn(diagram.toHTML).size == problem.graph.nodes.size)
    assert("<polygon ".r.findAllIn(diagram.toHTML).size == problem.graph.edges.size)
    assert(diagram.toString.linesIterator.count(_.startsWith("\\draw[->]")) == problem.graph.edges.size)
    assert(!diagram.toString.contains("node[midway"))
  }
}
