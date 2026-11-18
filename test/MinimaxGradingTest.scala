package info.kwarc.probgen

import org.scalatest.funsuite.AnyFunSuite

/** step B6 (Minimax): on random game trees, model answers typed in are graded correct,
  * and typical wrong answers are not
  */
class MinimaxGradingTest extends AnyFunSuite {

  private val runs = 60
  private def problems = List.fill(runs)(MinimaxProblemGenerator.make())
  private def hint(r: CheckResult): String = r match {
    case Incorrect(h) => h; case MalformedInput(h) => h; case PartiallyCorrect(_, h) => h; case o => o.toString
  }

  test("the minimax value of the root") {
    for (p <- problems) {
      val v = p.tree.rootValue
      val q = p.labelRoot
      assert(q.check(s"$v") == Correct())
      assert(q.check(s"${p.tree.root.name} = $v") == Correct())
      assert(q.check(s"${v + 1}").isInstanceOf[Incorrect])
      assert(q.check("big").isInstanceOf[MalformedInput])
    }
  }

  test("the chosen move: a node name, also as 'A -> B' and in lower case") {
    for (p <- problems) {
      val best = p.tree.uniqueBestMove.get
      val q = p.chooseMove
      assert(q.check(best) == Correct())
      assert(q.check(best.toLowerCase) == Correct())
      assert(q.check(s"${p.tree.root.name} -> $best") == Correct())
      p.tree.root.children.map(_.name).find(_ != best).foreach(other =>
        assert(hint(q.check(other)).contains("not the move")))
      assert(q.check("Z").isInstanceOf[MalformedInput]) // no such node (trees have at most 26 nodes, A..)
    }
  }

  test("pruned nodes: any order, partial credit, whole-subtree reminder") {
    for (p <- problems) {
      val pruned = p.tree.pruned
      val q = p.pruning
      assert(q.check(pruned.mkString(", ")) == Correct())
      assert(q.check(pruned.reverse.mkString("{", ", ", "}")) == Correct())
      if (pruned.length > 1) {
        val r = q.check(pruned.tail.mkString(", "))
        assert(r.isInstanceOf[PartiallyCorrect] && hint(r).contains("whole subtree"))
      }
      assert(q.check("none").isInstanceOf[Incorrect])
    }
  }

  test("labels that force the move: any condition that picks out the same labels") {
    var tested = 0
    for (p <- problems if p.labelsForcingMove.applicable()) {
      tested += 1
      val q = p.labelsForcingMove
      val leaf = p.hiddenLeaf.get
      val r = p.tree.labelsForcingMove(leaf, p.tree.moveContaining(leaf).get).get
      // the same set of labels, written in several ways
      val ways: List[String] = (r.lo, r.hi) match {
        case (Some(l), None)    => List(s"$leaf >= $l", s">= $l", s"$leaf > ${l - 1}", s"$l <= $leaf", s"$leaf ≥ $l", s"=> $l")
        case (None, Some(h))    => List(s"$leaf <= $h", s"<= $h", s"$leaf < ${h + 1}")
        case (Some(l), Some(h)) => List(s"$l <= $leaf <= $h", s"$leaf >= $l and $leaf <= $h") ++ (if (l == h) List(s"$l", s"$leaf = $l") else Nil)
        case (None, None)       => List("true")
      }
      for (w <- ways) assert(q.check(w) == Correct(), s"$w for $r")
      // off by one at the lower boundary
      r.lo.foreach(l => assert(hint(q.check(s"$leaf >= ${l + 1}" + r.hi.map(h => s" and $leaf <= $h").getOrElse(""))).contains("Almost")))
      assert(q.check(s"X >= 3").isInstanceOf[MalformedInput]) // not about this leaf
    }
    info(s"$tested trees had a hidden leaf")
  }
}
