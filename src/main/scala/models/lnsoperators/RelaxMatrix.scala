package models.lnsoperators

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPSetVar

import scala.util.Random

class RelaxMatrix(cp: CPSolver, nsubmatrices: Int,
                  nRows: Int, nCols: Int,
                  lines: Array[CPSetVar], cols: Array[CPSetVar],
                  currentSolLines: Array[Set[Int]], currentSolCols: Array[Set[Int]],
                  random: Random)
  extends Operator(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random) {

  var randomness = 0.5

  def run(): Unit = {
    //println("ININININ "+randomness)
    val o1 = cp.startSubjectTo(failureLimit = 1000) {
      for(i <- 0 until nRows) {
        if(random.nextDouble() > randomness)
          for(k <- 0 until nsubmatrices)
            constrain(lines(k), i, currentSolLines(k).contains(i))
      }
      for(j <- 0 until nCols) {
        if(random.nextDouble() > randomness)
          for(k <- 0 until nsubmatrices)
            constrain(cols(k), j, currentSolCols(k).contains(j))
      }
      cp.propagate()
    }

    //println("OUTOUTOUT")
    randomness = updateRandomness(randomness, o1.completed)
  }

  def name: String = "RelaxMatrix with random " + randomness
}
