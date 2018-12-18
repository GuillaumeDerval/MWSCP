package apps

import utils.Solution

object Solve extends SolveBase() {
  def newSol(sol: Solution): Unit = {
    println("New best bound: " + sol.obj)
    println("Time: " + sol.time)
    if(best != -1.0)
      println("Ratio and abs diff: "+ (sol.obj/best) + " " + (sol.obj - best))
    println("Rows [" + sol.rows.map(_.mkString(",")).mkString("],\t[") + "]")
    println("Cols [" + sol.cols.map(_.mkString(",")).mkString("],\t[") + "]")
    if (sol.comment != null)
      println("Comment " + sol.comment)
    println("---")
  }
  run()
}
