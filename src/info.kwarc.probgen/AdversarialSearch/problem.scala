package info.kwarc.probgen

import SText._
import Expr._

/** a minimax game tree problem, as in the adversarial search part of the exams
  *
  * The maximizing player is to move at the root and the leaves carry the values
  * of the static evaluation function. Like in the exams, the value of one leaf
  * may be missing from the picture (`hiddenLeaf`): the student is asked which
  * labels of that leaf would force a certain move, and the questions that need
  * a concrete value state the assumed label explicitly.
  */
case class MinimaxProblem(tree: MinimaxTree, hiddenLeaf: Option[String])
    extends Problem[MinimaxProblem] {

  private val rootName = tree.root.name

  def intro() = {
    val missing =
      if (hiddenLeaf.isEmpty) x""
      else x"; some of those values are currently missing"
    SSnippet(
      List(
        // both sentences in one interpolation: SSnippet does not put a space
        // between two nested snippets
        x"Consider the following minimax game tree for the maximizing player's turn. The values at the leaves are the static evaluation function values of those states$missing.",
        SGameTree(tree.root, hiddenLeaf.toSet)
      ),
      "\n"
    )
  }

  /** the exams hide one leaf value in the picture and fix it again before the
    * questions that need it, so every such question states the assumption
    */
  private def assumption(): SText = hiddenLeaf match {
    case None    => x""
    case Some(l) =>
      x"Assume ${Var(l)} is labeled with ${DInt(tree.values(l))}. "
  }

  // ── Parsing of the answers ───────────────────────────────────────────────

  /** all integers occurring in the input */
  private def ints(s: String): List[Int] =
    "-?\\d+".r.findAllIn(s).map(_.toInt).toList

  /** all node names occurring in the input, i.e. all single letters; words like
    * "and" or "node" are ignored
    */
  private def nodeNames(s: String): List[String] = {
    val tokens = s.toUpperCase.split("[^A-Z]+").filter(_.nonEmpty).toList
    // a run of letters like "IJLMO" is a list of nodes, but only if every
    // letter really is a node of this tree, so that words are not mistaken for
    // node names
    tokens.flatMap { t =>
      if (t.length == 1) List(t)
      else {
        val letters = t.map(_.toString).toList
        if (letters.forall(tree.names.contains)) letters else Nil
      }
    }.distinct
  }

  private def count(n: Int, noun: String) =
    if (n == 1) s"$n $noun" else s"$n ${noun}s"

  private def saysNone(s: String) = {
    val t = s.toLowerCase.trim
    t == "-" || t == "none" || t == "no" || t == "nothing" || t == "no nodes" ||
    t == "nothing is pruned" || t == "none are pruned"
  }

  /** reads an answer like ">= 4", "at least 4", "4+", "3..7" or "5" */
  private def parseRange(raw: String): Option[IntRange] = {
    val words = raw.toLowerCase
      .replace("≥", ">=")
      .replace("≤", "<=")
      .replace("=>", ">=")
      .replace("=<", "<=")
      .replace("at least", ">=")
      .replace("at most", "<=")
      .replace("greater than or equal to", ">=")
      .replace("less than or equal to", "<=")
      .replace("greater than", ">")
      .replace("bigger than", ">")
      .replace("larger than", ">")
      .replace("less than", "<")
      .replace("smaller than", "<")
      .replace("or more", "+")
      .replace("or greater", "+")
      .replace("or higher", "+")
      .replace("or above", "+")
    val s = words.filterNot(_.isWhitespace)
    val ns = ints(s)
    if (ns.isEmpty) None
    else if (s.contains("..") || s.contains("to")) {
      if (ns.length == 2) Some(IntRange(Some(ns.min), Some(ns.max))) else None
    } else
      List(">=", "<=", ">", "<").find(s.contains(_)) match {
        case None =>
          if (s.endsWith("+")) Some(IntRange(Some(ns.head), None))
          else if (ns.length == 1) Some(IntRange(Some(ns.head), Some(ns.head)))
          else None
        case Some(op) =>
          val i = s.indexOf(op)
          // "3 <= E" says the same as "E >= 3", so the operator has to be
          // flipped if the number stands on its left
          val flipped = s.take(i).exists(_.isDigit)
          val n = ints(s.drop(i)).headOption.getOrElse(ns.head)
          val effective =
            if (!flipped) op
            else
              op match {
                case ">=" => "<="
                case "<=" => ">="
                case ">"  => "<"
                case _    => ">"
              }
          effective match {
            case ">=" => Some(IntRange(Some(n), None))
            case "<=" => Some(IntRange(None, Some(n)))
            case ">"  => Some(IntRange(Some(n + 1), None))
            case _    => Some(IntRange(None, Some(n - 1)))
          }
      }
  }

  private def describeRange(r: IntRange, leaf: String): SText =
    (r.lo, r.hi) match {
      case (Some(l), None)    => x"every label with ${LessEq(DInt(l), Var(leaf))}"
      case (None, Some(h))    => x"every label with ${LessEq(Var(leaf), DInt(h))}"
      case (Some(l), Some(h)) =>
        if (l == h) x"only the label ${Equals(Var(leaf), DInt(l))}"
        else x"every label with ${LessEq(DInt(l), Var(leaf), DInt(h))}"
      case (None, None)       => x"every label"
    }

  // ── Subproblems ──────────────────────────────────────────────────────────

  /** the labels of the missing leaf that force one particular move */
  object labelsForcingMove extends Subproblem("minimax", 2, 4) {
    private def leaf = hiddenLeaf.get
    private def move = tree.moveContaining(leaf).get
    private def range = tree.labelsForcingMove(leaf, move).get

    override def applicable() =
      hiddenLeaf.isDefined &&
        tree.moveContaining(hiddenLeaf.get).isDefined &&
        tree
          .labelsForcingMove(hiddenLeaf.get, tree.moveContaining(hiddenLeaf.get).get)
          .isDefined

    def question() =
      x"Give every possible label for the node ${Var(leaf)} that would result in the player definitely choosing move ${Var(move)} (no matter how ties are broken)."

    def solution() = describeRange(range, leaf)

    override def checkSolution(input: String): CheckResult = {
      if (saysNone(input))
        return Incorrect("There are labels that force this move.")
      parseRange(input) match {
        case None        =>
          Incorrect("Give a range of labels, for example \">= 4\" or \"3..7\".")
        case Some(answer) =>
          val expected = range
          if (answer == expected) Correct()
          else if (answer.lo.isDefined && expected.lo.isDefined &&
            math.abs(answer.lo.get - expected.lo.get) == 1 && answer.hi == expected.hi)
            Incorrect(
              "Almost — check whether the boundary label itself already makes the move strictly better than the others."
            )
          else if (answer.hi.isDefined && expected.hi.isEmpty)
            Incorrect(
              "Higher labels are possible too: think about what the parent of this leaf does with a very large value."
            )
          else Incorrect("That is not the set of labels that force this move.")
      }
    }
  }

  /** the minimax value of the root */
  object labelRoot extends Subproblem("minimax", 2, 3) {
    def question() =
      assumption() + x"Label the node ${Var(rootName)} with its minimax value."
    def solution() = x"${Equals(Var(rootName), DInt(tree.rootValue))}"

    override def checkSolution(input: String): CheckResult =
      ints(input) match {
        case List(v) =>
          if (v == tree.rootValue) Correct()
          else
            Incorrect(
              "That is not the minimax value of the root. Work bottom up: maximize at even depth, minimize at odd depth."
            )
        case Nil => Incorrect("Give the minimax value as a number.")
        case _   => Incorrect("Give a single number.")
      }
  }

  /** the move the player chooses */
  object chooseMove extends Subproblem("minimax", 1, 3) {
    override def applicable() = tree.uniqueBestMove.isDefined

    def question() =
      assumption() + x"Which move would be chosen by the player?"
    def solution() = x"${Var(tree.uniqueBestMove.get)}"

    override def checkSolution(input: String): CheckResult = {
      // a student may write the move as "A -> B", so ignore the root
      val answer = nodeNames(input).filterNot(_ == rootName)
      answer match {
        case List(n) =>
          if (n == tree.uniqueBestMove.get) Correct()
          else if (!tree.root.children.exists(_.name == n))
            Incorrect(s"$n is not a move, i.e. not a child of the root.")
          else Incorrect("That is not the move the player would choose.")
        case Nil => Incorrect("Answer with the name of a node, for example B.")
        case _   => Incorrect("The player chooses exactly one move.")
      }
    }
  }

  /** the nodes that alpha-beta pruning prunes */
  object pruning extends Subproblem("alphabeta", 2, 4) {
    override def applicable() = tree.pruned.nonEmpty

    def question() =
      assumption() + x"Which nodes does $SAlphaBetaPruning prune? We expand child nodes in alphabetical order."
    def solution() = x"${FinSet(tree.pruned.map(Var(_))*)}"

    override def checkSolution(input: String): CheckResult = {
      val expected = tree.pruned.toSet
      if (saysNone(input))
        return Incorrect(s"${count(expected.size, "node")} are pruned.")
      val answer = nodeNames(input).toSet
      if (answer.isEmpty)
        return Incorrect("List the pruned nodes by name, for example \"F, G\".")
      val unknown = answer.filterNot(n => tree.names.contains(n))
      if (unknown.nonEmpty)
        return Incorrect(
          s"${unknown.toList.sorted.mkString(", ")} do(es) not occur in this tree."
        )
      if (answer == expected) Correct()
      else {
        val missing = (expected -- answer).size
        val extra = (answer -- expected).size
        val hints = List(
          if (missing > 0) Some(s"you are missing ${count(missing, "pruned node")}")
          else None,
          if (extra > 0)
            Some(s"${count(extra, "node")} you listed are actually visited")
          else None
        ).flatten
        Incorrect(
          "Not quite: " + hints.mkString(" and ") +
            ". Remember that pruning a node prunes its whole subtree."
        )
      }
    }
  }

  // Subproblems are objects and therefore only registered once they are
  // referenced; these lines both register them and fix the order in which they
  // are asked.
  GroupConstraint(1, 1, labelsForcingMove)
  GroupConstraint(1, 1, labelRoot)
  GroupConstraint(1, 1, chooseMove)
  GroupConstraint(1, 1, pruning)
}
