package info.kwarc.probgen

object Test {
  def main(args: Array[String]): Unit = {
    val p = SearchProblemGenerator.make()
    val s = p.toSTeX
    println(s)
  }
}