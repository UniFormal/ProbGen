package info.kwarc.probgen

import SText._

case class HeuristicSearchProblem(graph: HeuristicSearchGraph, positions: Map[String, (Double, Double)]) extends Problem[HeuristicSearchProblem] {
  def intro() = SSnippet(List(
    x"Each node shows its name and heuristic value. Each directed edge is labeled with its cost.",
    SHeuristicSearchGraph(graph, positions),
    x"Assume you have already expanded the root node A. The goal is node ${graph.nodes.map(_.name).max}. For the search-algorithm questions, list up to the next 4 nodes expanded by the respective algorithm. Stop when the goal is reached and expand each node at most once. If there is a tie, break it using alphabetical order."
  ), "\n")

  class ExpansionQuestion(val algorithm: HeuristicSearchAlgorithm)
      extends Subproblem(algorithm.toString, algorithm.points, 2) {
    lazy val expected: List[String] = HeuristicSearch.nextExpansions(graph, algorithm)
    override def answerHint = "up to four nodes in order, e.g. BCDE or B,C,D,E"
    def question() = x"${algorithm.title}"
    def solution() = SText(expected.mkString(", "))

    override def checkSolution(input: String): CheckResult = {
      val compact = input.filterNot(c => c.isWhitespace || Character.isSpaceChar(c))
      val normalized =
        if (compact.matches("[A-Za-z]+")) compact.toList.mkString(",")
        else compact
      val parsed = ExprParser.parseExprList(normalized).flatMap { terms =>
        Grader.sequence(terms) {
          case Var(name) if graph.nodes.exists(_.name == name.toUpperCase) => Right(name.toUpperCase)
          case _ => Left("Enter node names in order, e.g. BCDE or B,C,D,E.")
        }
      }
      Grader.onParsed(parsed) { answer =>
        if (answer == expected) Correct()
        else Incorrect("Check the expansion order. Start after A, break ties alphabetically, and expand each node only once.")
      }
    }
  }

  private def compact(input: String): String =
    input.filterNot(c => c.isWhitespace || Character.isSpaceChar(c)).toLowerCase

  private def saysInfinity(input: String): Boolean =
    Set("inf", "infinity", "∞").contains(compact(input))

  abstract class NodeQuestion(id: String) extends Subproblem(id, 2, 2) {
    var node: String = ""
    override def init(): Unit =
      node = Generator.choose(graph.nodes.map(_.name).filterNot(_ == graph.nodes.map(_.name).max))
    def minimum: Option[Int] = graph.minimumCostsToGoal(node)
  }

  object optimalCost extends NodeQuestion("optimal-cost") {
    override def answerHint = "a number, e.g. 7 or infinity"
    def question() = x"Give the value of h*(${node})"
    def solution() = SText(minimum.map(_.toString).getOrElse("infinity"))

    override def checkSolution(input: String): CheckResult = minimum match {
      case Some(value) =>
        if (saysInfinity(input)) Incorrect("The goal is reachable from this node.")
        else NumberGrader(value, tol = 0).grade(input)
      case None =>
        if (saysInfinity(input)) Correct()
        else Grader.onParsed(NumberGrader.parse(input))(_ =>
          Incorrect("The goal cannot be reached from this node, so its remaining cost is infinite."))
    }
  }

  object admissibleRange extends NodeQuestion("admissible-range") {
    override def answerHint = "a range, e.g. 0-3 or 5-infinity"
    def question() =
      x"What nonnegative values of h(${node}) keep h admissible?"
    def solution() = SText("0-" + minimum.map(_.toString).getOrElse("infinity"))

    override def checkSolution(input: String): CheckResult = {
      val range = """([0-9]+)\s*[-–]\s*([0-9]+|inf|infinity|∞)""".r
      compact(input) match {
        case range(lower, upper) =>
          val lowerValue = lower.toDouble
          val upperValue = if (saysInfinity(upper)) None else Some(upper.toDouble)
          if (lowerValue == 0 && upperValue == minimum.map(_.toDouble)) Correct()
          else Incorrect("The inclusive range starts at zero and ends at the true minimum remaining cost, or is unbounded if the goal is unreachable.")
        case _ => MalformedInput("Enter an inclusive range such as 0-6 or 0-infinity.")
      }
    }
  }

  object consistency extends Subproblem("consistency", 2, 2) {
    override def answerHint = "yes or a counter example, e.g. AB"
    def question() =
      x"Is the heuristic consistent? Answer yes if it is and give a counter example if it is not."
    def solution() =
      if (graph.inconsistentEdges.isEmpty) x"yes"
      else SText("No. Valid counterexamples include " +
        graph.inconsistentEdges.map(edge => edge.from + edge.to).mkString(", ") + ".")

    override def checkSolution(input: String): CheckResult = {
      val answer = compact(input)
      if (answer == "yes") {
        if (graph.inconsistentEdges.isEmpty) Correct()
        else Incorrect("At least one edge violates the consistency inequality.")
      } else if (answer.matches("[a-z]{2}")) {
        val from = answer.take(1).toUpperCase
        val to = answer.drop(1).toUpperCase
        if (!graph.edges.exists(edge => edge.from == from && edge.to == to))
          MalformedInput("That directed edge does not exist in this graph.")
        else if (graph.inconsistentEdges.exists(edge => edge.from == from && edge.to == to)) Correct()
        else Incorrect("This edge satisfies the consistency inequality.")
      } else MalformedInput("Answer yes or give one directed edge such as BC as a counterexample.")
    }
  }

  GroupConstraint(1, 1, optimalCost, admissibleRange)
  GroupConstraint(1, 1, consistency)

  val questions = HeuristicSearchAlgorithm.values.toList.map(new ExpansionQuestion(_))
  questions.foreach(question => GroupConstraint(1, 1, question))
}
