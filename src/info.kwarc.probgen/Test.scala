package info.kwarc.probgen

/** simple main method to call generators and see their results */
object Test {
  def main(args: Array[String]): Unit = {
    val p = SearchProblemGenerator.make()
    // val p = BasicProbabilityProblemGenerator.make()
    //val p = MDPGenerator.make()
    val subs = p.chooseSubproblems()
    val stex = p.toSTeX(subs)
    if (args.nonEmpty && args(0) == "--full") {
      val d = SDocument("problem", stex)
      println(d.toStringFull)
    } else {
      println(stex)
    }
  }
}