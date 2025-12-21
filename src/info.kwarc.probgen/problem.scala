package info.kwarc.probgen

trait Problem[PD <: Problem[PD]] {
  def intro(): SText

  private var subproblems: List[Subproblem] = Nil
  private var groupConstraints: List[GroupConstraint] = Nil

  abstract class Subproblem(val id: String, pts: Int, testspace: Int) {
    subproblems = subproblems ::: List(this)
    def dependencies: List[String] = Nil
    def applicable(): Boolean = true

    def init(): Unit = {}

    def question(): SText
    def solution(): SText
    def toSTeX() = {
      SSubproblem(pts, question(), SSolution(testspace, List(solution())))
    }
  }

  case class GroupConstraint(atLeast: Int, atMost: Int, choices: Subproblem*) {
    groupConstraints ::= this
    val length = choices.length
  }

  def chooseSubproblems(): List[Subproblem] = {
    var subs = subproblems.filter(_.applicable())
    groupConstraints.foreach {gc =>
      val currentlyChosen = subs.filter(p => gc.choices.contains(p))
      if (currentlyChosen.length > gc.atMost) {
        val numRemove = currentlyChosen.length - gc.atMost
        val remove = Generator.chooseSome(currentlyChosen, numRemove,numRemove)
        subs = subs.diff(remove)
      }
    }
    subs.map(_.init())
    subs
  }

  def toSTeX(subs: List[Subproblem]) = {
    SProblem(intro, subs.map(_.toSTeX))
  }
  def toSTeXAll() = {
    val subs = subproblems
    subs.map(_.init())
    toSTeX(subs)
  }
}
