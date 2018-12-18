package models.lnsoperators

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPSetVar

import scala.util.Random

class OnlyPossibleLines(cp: CPSolver, nsubmatrices: Int,
                        nRows: Int, nCols: Int,
                        lines: Array[CPSetVar], cols: Array[CPSetVar],
                        currentSolLines: Array[Set[Int]], currentSolCols: Array[Set[Int]],
                        random: Random)
  extends Operator(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random) {

  var randomness = 0.3

  def run(): Unit = {
    val possibleRows = currentSolLines.reduceLeft((cur, next) => cur ++ next)
    val possibleCols = currentSolCols.reduceLeft((cur, next) => cur ++ next)
    val o1 = cp.startSubjectTo(failureLimit = 1000) {
      for (k <- 0 until nsubmatrices) {
        for (i <- 0 until nRows) {
          if (!possibleRows.contains(i))
            lines(k).excludes(i)
          else
            constrainRandomly(lines(k), i, currentSolLines(k).contains(i), randomness)
        }
        for (j <- 0 until nCols) {
          if (!possibleCols.contains(j))
            cols(k).excludes(j)
          else
            constrainRandomly(cols(k), j, currentSolCols(k).contains(j), randomness)
        }
      }
      cp.propagate()
    }
    if (o1.completed)
      randomness *= 1.1
    else
      randomness /= 1.1
  }

  def name: String = "Only possible lines"
}
