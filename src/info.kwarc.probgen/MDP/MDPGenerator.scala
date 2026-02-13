package info.kwarc.probgen

import info.kwarc.probgen.SText._ // FIX: Required for x"..."

object MDPGenerator {
  
  def make(): MDPProblem = {
    var good = false
    var problem: MDPProblem = null
    var attempts = 0

    println("--- Starting Generator ---")

    while (!good) {
      attempts += 1
      val isGrid = Generator.chooseBoolean(0.5) 

      // 1. Build and Solve inside the specific branch to preserve types
      val (mdp, desc, solution, start, steps) = if (isGrid) {
        val w = Generator.chooseInt(3, 4)
        val h = Generator.chooseInt(3, 3) 
        val m = GridMDP(w, h)
        val d = SText(x" A $w \\times $h grid. Start at (0,0). Goal at (${w-1}, ${h-1}). Actions: Up, Down, Left, Right. Success prob 0.8.")
        val s = (0,0)
        
        val sol = m.solve()
        val pol = m.getOptimalPolicy(sol)
        val stp = m.stepsToGoal(s, pol)
        
        (m, d, sol.asInstanceOf[Map[Any, Double]], s, stp)
      } else {
        val n = Generator.chooseInt(6, 8)
        val m = CircularMDP(n, 0.8, false)
        val d = SText(x" A cyclic track of $n states (0 to ${n-1}). Goal is 0. Actions: +1 (Right), -1 (Left).")
        val s = Generator.chooseInt(n/2, n-1)
        
        val sol = m.solve()
        val pol = m.getOptimalPolicy(sol)
        val stp = m.stepsToGoal(s, pol)
        
        (m, d, sol.asInstanceOf[Map[Any, Double]], s, stp)
      }
      
      // 2. Choose Mode
      val mode = if (Generator.chooseBoolean(0.5)) "VI" else "PI"
      
      val mdpAny = mdp.asInstanceOf[MDP[Any, Any]]
      val initialPolicy = if (mode == "PI") {
         mdpAny.states.map(s => s -> mdpAny.actions.head).toMap
      } else Map()

      // 3. Validate
      // steps > 1 ensures we aren't already at the goal
      val distinctValues = solution.values.toSet.size
      
      if (distinctValues >= 2 && steps > 1 && steps < 8) {
        println(s"Found good problem after $attempts attempts. (Mode: $mode, Steps: $steps)")
        problem = MDPProblem(mdp, desc, solution, start, mode, initialPolicy)
        good = true
      }
    }
    problem
  }
}