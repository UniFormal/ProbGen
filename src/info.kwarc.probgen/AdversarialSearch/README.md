# Adversarial Search Problem Generator

Generates the minimax game tree problems from the adversarial search part of the
exams: a tree for the maximizing player's turn, leaves labeled with the values of
the static evaluation function, nodes named `A`, `B`, `C`, ... in breadth-first
order, and the value of one leaf optionally missing from the picture.

The generated questions are the ones the exams keep asking:

1. the labels of the missing leaf that force one particular move
   (only if a leaf value is missing)
2. the minimax value of the root
3. the move the player chooses
4. the nodes that αβ-pruning prunes, expanding children in alphabetical order

## Files

| file | contents |
| --- | --- |
| `logic.scala` | the game tree and the algorithms: minimax, αβ-pruning, relabeling |
| `rendering.scala` | draws a tree as SVG (browser) and as tikz (exam sheet) |
| `problem.scala` | the questions, their solutions, and the answer checking |
| `generator.scala` | random trees plus the criteria a good tree has to satisfy |

Nothing outside this folder had to be changed except for one line in
`main.scala` that registers the problem.

## `logic.scala`

A tree is a `GameLeaf(name, value)` or a `GameNode(name, children)`. Leaves may
sit at any depth, so the tree does not have to be balanced — the exams always
expand some states one or two levels further than others.

`MinimaxTree` wraps a tree and computes everything on demand:

* `values` — the minimax value of every node. Nodes at even depth belong to the
  maximizing player, nodes at odd depth to the minimizing player.
* `uniqueBestMove` — the child of the root that is strictly better than all
  others, i.e. the move the player takes no matter how ties are broken. `None`
  if two moves are tied.
* `visited` / `pruned` — αβ-pruning expands children from left to right, which
  is alphabetical order because of the breadth-first naming. Every node the
  algorithm is never called on counts as pruned, so pruning a node prunes its
  whole subtree.
* `withLeaf(leaf, v)` — the same tree with one leaf relabeled, which is what the
  questions about changed labels are built on.
* `labelsForcingMove(leaf, move)` — the labels of `leaf` that make `move`
  strictly best. The minimax value is monotone in every leaf, so this set is
  always an interval, and it is found by scanning `MinimaxTree.scanFrom` to
  `MinimaxTree.scanTo`; the bounds of that scan stand for "unbounded".

`MinimaxTree.named` renames a tree to `A`, `B`, `C`, ... breadth first, which is
what makes "expand children in alphabetical order" well defined.

## `rendering.scala`

`SGameTree` implements the framework's `SText`, so it can be dropped into an
`x"..."` interpolation like any other piece of text.

The layout is computed once: every leaf gets its own column from left to right,
and every inner node is centered over its children. Both backends are generated
from that same layout, so the browser and the exam sheet show the same tree.

* `toHTML` — an inline SVG. Edges are drawn from centre to centre first and the
  filled circles then cover their ends.
* `toString` — a tikz picture with explicit coordinates. `stexlight.sty` already
  loads tikz, so no extra package is needed. For wide trees the column width
  shrinks so that the picture stays inside the text width.

`SAlphaBetaPruning` renders "αβ-pruning" in both backends; the framework has no
LaTeX command for α and β.

## `problem.scala`

`MinimaxProblem(tree, hiddenLeaf)` holds the complete tree — `hiddenLeaf` only
says whose value is left out of the *picture*. Every question that needs the
value states it explicitly ("Assume E is labeled with 5"), exactly like the
exams do, so that each question stands on its own.

The answers are checked by verifying the property, not by comparing strings, so
every correct formulation is accepted:

| question | accepted |
| --- | --- |
| minimax value | `4`, `A = 4` |
| chosen move | `B`, `move B`, `A -> B` |
| pruned nodes | `{I, J, L}`, `L J I`, `IJL` — order does not matter |
| forcing labels | `>= 3`, `at least 3`, `3+`, `> 2`, `3 <= G` |

Wrong answers get a hint that does not give the solution away, e.g. how many
pruned nodes are missing.

## `generator.scala`

Builds a random tree — a few moves, a few successors each, and one or two of
those states expanded further — and keeps it only if

* the player has a unique best move, so that "which move" has one answer,
* αβ-pruning prunes at least `minPrunedNodes` nodes but at most
  `maxPrunedShare` of the tree, so the question is neither trivial nor
  degenerate,
* the tree has at most `maxLeaves` leaves, since the picture needs one column
  per leaf.

A leaf value is left out of the picture with probability `hiddenLeafChance`, and
only for a leaf whose forcing labels form a real threshold within the range of
the values used in the tree.

All parameters are the constants at the top of the object.
