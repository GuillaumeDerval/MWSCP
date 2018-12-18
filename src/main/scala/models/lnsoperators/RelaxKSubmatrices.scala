package models.lnsoperators

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPSetVar

import scala.util.Random

class RelaxKSubmatrices(cp: CPSolver, nsubmatrices: Int,
                        nRows: Int, nCols: Int,
                        lines: Array[CPSetVar], cols: Array[CPSetVar],
                        currentSolLines: Array[Set[Int]], currentSolCols: Array[Set[Int]],
                        random: Random,
                        relaxN: Int = 2)
  extends Operator(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random) {

  var randomness = 0.5

  val allSubM: List[Int] = (0 until nsubmatrices).toList

  def run(): Unit = {
    val selected = random.shuffle(allSubM).slice(0, relaxN).toSet
    val possibleRows = selected.map(currentSolLines(_)).reduceLeft((cur, next) => cur ++ next)
    val possibleCols = selected.map(currentSolCols(_)).reduceLeft((cur, next) => cur ++ next)

    val o1 = cp.startSubjectTo(failureLimit = 1000) {
      for (k <- 0 until nsubmatrices) {
        if (!selected.contains(k)) {
          for (i <- 0 until nRows)
            constrain(lines(k), i, currentSolLines(k).contains(i))
          for (j <- 0 until nCols)
            constrain(cols(k), j, currentSolCols(k).contains(j))
        }
        else {
          for (i <- 0 until nRows) {
            if (possibleRows.contains(i))
              constrainRandomly(lines(k), i, currentSolLines(k).contains(i), randomness)
            else
              lines(k).excludes(i)
          }
          for (j <- 0 until nCols) {
            if (possibleCols.contains(j))
              constrainRandomly(cols(k), j, currentSolCols(k).contains(j), randomness)
            else
              cols(k).excludes(j)
          }
        }
      }
      cp.propagate()
    }
    randomness = updateRandomness(randomness, o1.completed)
    //println(o1)
  }

  def name: String = "RelaxK with k="+relaxN+" with random " + randomness
}
