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

  // ── Reading the answers ──────────────────────────────────────────────────

  /** a node of this tree, as typed by the student (case does not matter) */
  private def nodeOf(t: Term): Either[String, String] = t match {
    case Var(n) if tree.names.contains(n.toUpperCase) => Right(n.toUpperCase)
    case other => Left(s"<code>${Html.esc(ExprCompare.show(other))}</code> is not a node of this tree. " +
      "Write node names separated by commas, e.g. F, G.")
  }

  /** the labels of `leaf` for which the student's condition holds, e.g. "E >= 4",
    * ">= 4" (the leaf is implied) or "5" (only that label)
    */
  private def labelsSatisfying(input: String, leaf: String): Either[String, Set[Int]] = {
    val t = input.trim.replace("=>", ">=").replace("=<", "<=")
    val text = if (t.headOption.exists(c => "<>=≤≥≠!".contains(c))) s"$leaf $t" else t
    val condition: Either[String, Form] = ExprParser.parseForm(text) match {
      case r @ Right(_) => r
      case Left(err) => ExprParser.parseTerm(t) match {
        case Right(n) if ExprCompare.numericValue(n).isDefined => Right(Equals(Var(leaf), n))
        case _                                                  => Left(err)
      }
    }
    val example = s"Write a condition on the label of $leaf, e.g. $leaf >= 4 or 3 <= $leaf <= 7."
    condition.flatMap { f =>
      if (!f.fvs.forall(_.toUpperCase == leaf)) Left(example)
      else
        try {
          val labels = Range(MinimaxTree.scanFrom, MinimaxTree.scanTo + 1).toList
          Right(labels.filter { v =>
            Evaluator(f)(using Context(List(leaf -> DInt(v), leaf.toLowerCase -> DInt(v))))
          }.toSet)
        } catch { case _: EvalError => Left(example) }
    }
  }

  private def count(n: Int, noun: String) =
    if (n == 1) s"$n $noun" else s"$n ${noun}s"

  private def saysNone(s: String) = {
    val t = s.toLowerCase.trim
    t == "-" || t == "none" || t == "no" || t == "nothing" || t == "no nodes" ||
    t == "nothing is pruned" || t == "none are pruned"
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
    override def answerHint = s"e.g. $leaf >= 4, or 3 <= $leaf <= 7"
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

    // right if the condition holds for exactly the labels that force the move
    override def checkSolution(input: String): CheckResult =
      if (saysNone(input)) Incorrect("There are labels that force this move.")
      else Grader.onParsed(labelsSatisfying(input, leaf)) { answered =>
        val labels   = Range(MinimaxTree.scanFrom, MinimaxTree.scanTo + 1).toList
        val expected = labels.filter(range.contains).toSet
        val differ   = (answered diff expected) union (expected diff answered)
        if (differ.isEmpty) Correct()
        else if (differ.size == 1 && range.lo.exists(l => differ.head == l || differ.head == l - 1))
          Incorrect("Almost — check whether the boundary label itself already makes the move strictly better than the others.")
        else if (range.hi.isEmpty && !answered.contains(MinimaxTree.scanTo))
          Incorrect("Higher labels are possible too: think about what the parent of this leaf does with a very large value.")
        else Incorrect("That is not the set of labels that force this move.")
      }
  }

  /** the minimax value of the root */
  object labelRoot extends Subproblem("minimax", 2, 3) {
    override def answerHint = "a number, e.g. 5"
    def question() =
      assumption() + x"Label the node ${Var(rootName)} with its minimax value."
    def solution() = x"${Equals(Var(rootName), DInt(tree.rootValue))}"

    override def checkSolution(input: String): CheckResult =
      Grader.onParsed(NumberGrader.parse(input)) { v =>
        if (v == tree.rootValue) Correct()
        else Incorrect("That is not the minimax value of the root. Work bottom up: maximize at even depth, minimize at odd depth.")
      }
  }

  /** the move the player chooses */
  object chooseMove extends Subproblem("minimax", 1, 3) {
    override def answerHint = "a node, e.g. B"
    override def applicable() = tree.uniqueBestMove.isDefined

    def question() =
      assumption() + x"Which move would be chosen by the player?"
    def solution() = x"${Var(tree.uniqueBestMove.get)}"

    // a student may write the move as "A -> B", from the root
    override def checkSolution(input: String): CheckResult = {
      val node: Either[String, String] = ExprParser.parseTerm(input) match {
        case Right(t) => nodeOf(t)
        case Left(err) => ExprParser.parseForm(input) match {
          case Right(Implies(Seq(BVar(from), BVar(to)))) if from.toUpperCase == rootName => nodeOf(Var(to))
          case _ => Left("Answer with the name of one node, for example B.")
        }
      }
      Grader.onParsed(node) { n =>
        if (n == tree.uniqueBestMove.get) Correct()
        else if (!tree.root.children.exists(_.name == n)) Incorrect(s"$n is not a move, i.e. not a child of the root.")
        else Incorrect("That is not the move the player would choose.")
      }
    }
  }

  /** the nodes that alpha-beta pruning prunes */
  object pruning extends Subproblem("alphabeta", 2, 4) {
    override def answerHint = "e.g. F, G, K"
    override def applicable() = tree.pruned.nonEmpty

    def question() =
      assumption() + x"Which nodes does $SAlphaBetaPruning prune? We expand child nodes in alphabetical order."
    def solution() = x"${FinSet(tree.pruned.map(Var(_))*)}"

    override def checkSolution(input: String): CheckResult = {
      val expected = tree.pruned.toSet
      if (saysNone(input)) Incorrect(s"${count(expected.size, "node")} are pruned.")
      else Grader.onParsed(ExprParser.parseTermList(input).flatMap(ts => Grader.sequence(ts)(nodeOf))) { answer =>
        Grader.toResult(Grader.compareSets(answer, expected, identity, false, "node")) match {
          case Incorrect(h)           => Incorrect(h + " Remember that pruning a node prunes its whole subtree.")
          case PartiallyCorrect(f, h) => PartiallyCorrect(f, h + " Remember that pruning a node prunes its whole subtree.")
          case r                      => r
        }
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
