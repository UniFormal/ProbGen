package info.kwarc.probgen

/** randomly generates a minimax game tree according to some criteria */
object MinimaxProblemGenerator extends ProblemGenerator[MinimaxProblem] {

  /** modify these to guide selection
    *
    * The shape follows the exams: the player has a few moves, every move leads
    * to a state with a few successors, and some of those states are expanded
    * one or two levels further instead of being evaluated right away.
    */

  // params for random generation, used once per tree
  val minMoves = 3
  val maxMoves = 3
  val minChildren = 2
  val maxChildren = 3
  val minLeafValue = 1
  val maxLeafValue = 10

  // how many states at depth 2 are expanded further, and how likely it is that
  // one state at depth 3 is expanded as well
  val minExpansions = 1
  val maxExpansions = 2
  val deepExpansionChance = 0.4

  // how often the picture leaves out the value of one leaf
  val hiddenLeafChance = 0.5

  // params for the criteria a tree has to satisfy
  val minPrunedNodes = 2
  val maxPrunedShare = 0.5

  // the picture has one column per leaf, so this keeps the tree printable
  val maxLeaves = 12

  def make(): MinimaxProblem = {
    var attempts = 0
    var problem: MinimaxProblem = null
    while (problem == null) {
      attempts += 1
      val raw = randomTree()
      if (raw.subtree.length <= 26) {
        val tree = MinimaxTree(MinimaxTree.named(raw))
        if (isGood(tree)) {
          val hidden =
            if (Generator.chooseBoolean(hiddenLeafChance)) chooseHiddenLeaf(tree)
            else None
          problem = MinimaxProblem(tree, hidden)
        }
      }
    }
    log(
      "Found good game tree after " + attempts + " attempts. (nodes: " +
        problem.tree.nodes.length + ", pruned: " + problem.tree.pruned.length +
        ", hidden leaf: " + problem.hiddenLeaf.getOrElse("none") + ")"
    )
    problem
  }

  /** builds a random tree with placeholder names, [[MinimaxTree.named]] renames
    * the nodes to A, B, C, ... afterwards
    */
  def randomTree(): GameTree = {
    var counter = 0
    def fresh() = { counter += 1; "n" + counter }
    def leaf() = GameLeaf(fresh(), Generator.chooseInt(minLeafValue, maxLeafValue))
    def leaves() =
      Range(0, Generator.chooseInt(minChildren, maxChildren)).toList.map(_ => leaf())

    /** replaces one leaf by an inner node, i.e. expands that state */
    def expand(t: GameTree, target: String): GameTree = t match {
      case GameLeaf(n, _) if n == target => GameNode(n, leaves())
      case GameNode(n, cs)               => GameNode(n, cs.map(c => expand(c, target)))
      case l                             => l
    }

    /** the leaves at a given depth */
    def leavesAt(t: GameTree, depth: Int): List[String] = {
      val mt = MinimaxTree(t)
      mt.leaves.map(_.name).filter(n => mt.depths(n) == depth)
    }

    val moves = Range(0, Generator.chooseInt(minMoves, maxMoves)).toList
      .map(_ => GameNode(fresh(), leaves()))
    var tree: GameTree = GameNode(fresh(), moves)

    /** the moves that do not lead to an expanded state yet */
    def unexpandedMoves(t: GameTree): List[String] =
      t.children.filter(m => m.children.forall(_.isLeaf)).map(_.name)

    Range(0, Generator.chooseInt(minExpansions, maxExpansions)).foreach { _ =>
      val cs = leavesAt(tree, 2)
      // spread the expansions over different moves, so that the tree does not
      // become lopsided
      val free = unexpandedMoves(tree)
      val preferred =
        cs.filter(l => MinimaxTree(tree).moveContaining(l).exists(free.contains))
      val choices = if (preferred.nonEmpty) preferred else cs
      if (choices.nonEmpty) tree = expand(tree, Generator.choose(choices))
    }
    if (Generator.chooseBoolean(deepExpansionChance)) {
      val cs = leavesAt(tree, 3)
      if (cs.nonEmpty) tree = expand(tree, Generator.choose(cs))
    }
    tree
  }

  /** we only want trees where the player has a clear preference and where
    * alpha-beta pruning does something, but not so much that most of the tree
    * disappears
    */
  def isGood(t: MinimaxTree): Boolean = {
    t.maxDepth >= 3 &&
    t.leaves.length <= maxLeaves &&
    t.uniqueBestMove.isDefined &&
    t.pruned.length >= minPrunedNodes &&
    t.pruned.length <= t.nodes.length * maxPrunedShare
  }

  /** picks a leaf whose value we leave out of the picture
    *
    * The labels of that leaf that force the player to choose the move it
    * belongs to must form a non-trivial threshold, otherwise the question about
    * them is either impossible or trivial.
    */
  def chooseHiddenLeaf(t: MinimaxTree): Option[String] = {
    val candidates = t.leaves.map(_.name).filter { l =>
      t.depths(l) >= 2 && (t.moveContaining(l) match {
        case None    => false
        case Some(m) =>
          t.labelsForcingMove(l, m) match {
            case Some(IntRange(Some(lo), _)) =>
              minLeafValue - 1 <= lo && lo <= maxLeafValue + 1
            case _ => false
          }
      })
    }
    if (candidates.isEmpty) None else Some(Generator.choose(candidates))
  }
}
