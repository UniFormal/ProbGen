package info.kwarc.probgen

object Test {
  def main(args: Array[String]): Unit = {
    val p = SearchProblemGenerator.make()
    if (p != null) {
      val subs = p.chooseSubproblems()
      val stex = p.toSTeX(subs)
      println(stex)

    }
  }
}