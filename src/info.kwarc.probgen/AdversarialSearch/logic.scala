package info.kwarc.probgen

/** a node of a minimax game tree
  *
  * An inner node is a state in which it is one player's turn, a leaf is a state
  * that we do not expand any further and that we evaluate with the static
  * evaluation function.
  */
sealed abstract class GameTree {
  def name: String
  def children: List[GameTree]
  def isLeaf = children.isEmpty

  /** this node and all its descendants, depth first */
  def subtree: List[GameTree] = this :: children.flatMap(_.subtree)
}
case class GameLeaf(name: String, value: Int) extends GameTree {
  def children = Nil
}
case class GameNode(name: String, children: List[GameTree]) extends GameTree

/** a set of integers of the form {v | lo <= v <= hi}, where None means
  * unbounded in that direction
  */
case class IntRange(lo: Option[Int], hi: Option[Int]) {
  def contains(v: Int) = lo.forall(_ <= v) && hi.forall(v <= _)
}

/** a minimax game tree in which the maximizing player is to move at the root,
  * as defined in the lecture
  *
  * Nodes at even depth belong to the maximizing player, nodes at odd depth to
  * the minimizing player. Leaves may occur at any depth, so the tree does not
  * have to be balanced.
  */
case class MinimaxTree(root: GameTree) {

  lazy val nodes: List[GameTree] = root.subtree
  lazy val names: List[String] = nodes.map(_.name).sorted
  lazy val leaves: List[GameLeaf] = nodes.collect { case l: GameLeaf => l }

  /** the depth of every node, the root has depth 0 */
  lazy val depths: Map[String, Int] = {
    def go(t: GameTree, d: Int): List[(String, Int)] =
      (t.name, d) :: t.children.flatMap(c => go(c, d + 1))
    go(root, 0).toMap
  }
  lazy val maxDepth = depths.values.max

  /** true if the maximizing player is to move in this node */
  def isMaxNode(n: String) = depths(n) % 2 == 0

  /** the minimax value of every node */
  lazy val values: Map[String, Int] = {
    var acc = Map[String, Int]()
    def go(t: GameTree, d: Int): Int = {
      val v = t match {
        case GameLeaf(_, value) => value
        case GameNode(_, cs)    =>
          val below = cs.map(c => go(c, d + 1))
          if (d % 2 == 0) below.max else below.min
      }
      acc += (t.name -> v)
      v
    }
    go(root, 0)
    acc
  }
  lazy val rootValue = values(root.name)

  /** the moves, i.e. the children of the root, that achieve the root's value */
  lazy val bestMoves: List[String] =
    root.children.filter(c => values(c.name) == rootValue).map(_.name)

  /** the move the player takes, unless ties make that ambiguous */
  lazy val uniqueBestMove: Option[String] =
    if (bestMoves.length == 1) Some(bestMoves.head) else None

  /** the move whose subtree contains the given node */
  def moveContaining(n: String): Option[String] =
    root.children.find(_.subtree.exists(_.name == n)).map(_.name)

  /** the nodes that alpha-beta pruning actually looks at
    *
    * Children are expanded from left to right, which is alphabetical order
    * because the nodes are named in breadth-first order.
    */
  lazy val visited: Set[String] = {
    var seen = Set[String]()
    def ab(t: GameTree, d: Int, alpha: Int, beta: Int): Int = {
      seen += t.name
      t match {
        case GameLeaf(_, v)  => v
        case GameNode(_, cs) =>
          var a = alpha
          var b = beta
          var rest = cs
          if (d % 2 == 0) {
            var v = Int.MinValue
            while (rest.nonEmpty) {
              v = math.max(v, ab(rest.head, d + 1, a, b))
              rest = rest.tail
              // the minimizing parent would never let us get here, so we stop
              if (v >= b) rest = Nil else a = math.max(a, v)
            }
            v
          } else {
            var v = Int.MaxValue
            while (rest.nonEmpty) {
              v = math.min(v, ab(rest.head, d + 1, a, b))
              rest = rest.tail
              if (v <= a) rest = Nil else b = math.min(b, v)
            }
            v
          }
      }
    }
    ab(root, 0, Int.MinValue, Int.MaxValue)
    seen
  }

  /** the nodes that alpha-beta pruning never looks at */
  lazy val pruned: List[String] = names.filterNot(visited.contains)

  /** the same tree with one leaf relabeled */
  def withLeaf(leaf: String, v: Int): MinimaxTree = {
    def go(t: GameTree): GameTree = t match {
      case GameLeaf(n, _) if n == leaf => GameLeaf(n, v)
      case GameNode(n, cs)             => GameNode(n, cs.map(go))
      case l                           => l
    }
    MinimaxTree(go(root))
  }

  /** the labels for `leaf` that make the player definitely choose `move`,
    * i.e. that make `move` strictly better than every other move
    *
    * The minimax value is monotone in every leaf, so this set is always an
    * interval; None means that no label achieves it.
    */
  def labelsForcingMove(leaf: String, move: String): Option[IntRange] = {
    val from = MinimaxTree.scanFrom
    val to = MinimaxTree.scanTo
    val good = Range(from, to + 1).toList.filter { v =>
      withLeaf(leaf, v).uniqueBestMove.contains(move)
    }
    if (good.isEmpty) None
    else
      Some(
        IntRange(
          if (good.min == from) None else Some(good.min),
          if (good.max == to) None else Some(good.max)
        )
      )
  }
}

object MinimaxTree {

  /** the range of labels we consider when looking for the labels that force a
    * move; the bounds stand for "unbounded", so they must be far outside the
    * range of the values actually used in the trees
    */
  val scanFrom = -99
  val scanTo = 99

  /** the node names A, B, C, ... in breadth-first order */
  def letter(i: Int) = (65 + i).toChar.toString

  /** renames all nodes to A, B, C, ... in breadth-first order, the way the
    * nodes are named in the exams
    */
  def named(t: GameTree): GameTree = {
    var order = Map[String, Int]()
    var level = List(t)
    while (level.nonEmpty) {
      level.foreach { n => order += (n.name -> order.size) }
      level = level.flatMap(_.children)
    }
    def go(t: GameTree): GameTree = t match {
      case GameLeaf(n, v)  => GameLeaf(letter(order(n)), v)
      case GameNode(n, cs) => GameNode(letter(order(n)), cs.map(go))
    }
    go(t)
  }
}
