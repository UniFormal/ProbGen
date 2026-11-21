package info.kwarc.probgen

/** the "αβ-pruning" of the lecture
  *
  * α and β are two separate identifiers, so they are rendered as two <mi>s
  * (a single Var("\\alpha\\beta") would be one upright two-letter name).
  * The "-pruning" is part of this node so that the text after it starts with a
  * space and [[SSnippet]] does not insert one before the hyphen.
  */
case object SAlphaBetaPruning extends SText {
  override def toString = "$\\alpha\\beta$-pruning"
  def toHTML = s"<math>${Name.html("\\alpha")}${Name.html("\\beta")}</math>-pruning"
}

/** Adapts minimax labels to the shared tree renderer. */
case class SGameTree(tree: GameTree, hidden: Set[String] = Set()) extends SText {
  private val diagram = STreeDiagram[GameTree](
    tree, _.name, _.children,
    {
      case GameLeaf(n, v) if !hidden.contains(n) => Some(v)
      case _ => None
    },
    _ => None, (_, _) => None
  )
  def toHTML = diagram.toHTML
  override def toString = diagram.toString
}
