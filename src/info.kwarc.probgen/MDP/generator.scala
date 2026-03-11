package info.kwarc.probgen

object MDPGenerator extends ProblemGenerator[MDPProblem] {
  
  def make(): MDPProblem = {
    var good = false
    var problem: MDPProblem = null
    var attempts = 0

    log("--- Starting Generator ---")

    while (!good) {
      attempts += 1
      val isGrid = Generator.chooseBoolean(0.5) 

      // 1. Build and Solve inside the specific branch to preserve types
      val succProb = Generator.choose(List(0.6,0.7,0.8,0.9))
      val mdp = if (isGrid) {
        val w = Generator.chooseInt(3, 4)
        val h = Generator.chooseInt(3, 3) 
        GridMDP(w, h, succProb)
      } else {
        val n = Generator.chooseInt(6, 8)
        val s = Generator.chooseInt(n/2, n-1)
        CircularMDP(n, s, succProb)
      }
      val pol = mdp.getOptimalPolicy()
      val steps = mdp.stepsToGoal(mdp.initial, pol)
      
      // 2. Choose Mode
      val isPolIter = Generator.chooseBoolean(0.5)

      // 3. Validate
      // steps > 1 ensures we aren't already at the goal
      val distinctValues = mdp.utilities.values.toSet.size
      
      if (distinctValues >= 2 && steps > 1 && steps < 8) {
        log(s"Found good problem after $attempts attempts. (pol. iter.: $isPolIter, steps: $steps)")
        problem = MDPProblem(mdp, isPolIter)
        good = true
      }
    }
    problem
  }
}