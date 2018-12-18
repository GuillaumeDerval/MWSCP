package models.lnsoperators

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPSetVar

import scala.util.Random

class Std(cp: CPSolver, nsubmatrices: Int,
          nRows: Int, nCols: Int,
          lines: Array[CPSetVar], cols: Array[CPSetVar],
          currentSolLines: Array[Set[Int]], currentSolCols: Array[Set[Int]],
          random: Random)
  extends Operator(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random) {

  var randomness = 1.0

  def run(): Unit = {
    val o1 = cp.startSubjectTo(failureLimit = 1000) {
      for (k <- 0 until nsubmatrices) {
        for (i <- 0 until nRows)
          constrainRandomly(lines(k), i, currentSolLines(k).contains(i), randomness)
        for (j <- 0 until nCols)
          constrainRandomly(cols(k), j, currentSolCols(k).contains(j), randomness)
      }
      cp.propagate()
    }

    if (randomness == 1.0)
      randomness = 0.30
    else if (o1.completed)
      randomness += Math.min(1.0-randomness, randomness) * 0.05
    else
      randomness -= Math.min(1.0-randomness, randomness) * 0.05
  }

  def name: String = "Std"
}
