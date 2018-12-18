package models.lnsoperators

import oscar.algo.Inconsistency
import oscar.cp.CPSolver
import oscar.cp.core.variables.CPSetVar

import scala.util.Random

class RelaxKSubmatricesM(cp: CPSolver, nsubmatrices: Int,
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

    val onlyRow = random.nextBoolean()

    val o1 = cp.startSubjectTo(failureLimit = 1000) {
      for (i <- 0 until nRows) {
        if (random.nextDouble() > randomness) {
          for (k <- selected) {
            constrain(lines(k), i, currentSolLines(k).contains(i))
            //print(k,"r",i,currentSolLines(k).contains(i))
          }
        }
        else if (!possibleRows.contains(i)) {
          for (k <- selected) {
            lines(k).excludes(i)
            //println(k,"r",i,false)
          }
        }
      }
      for (j <- 0 until nCols) {
        if (random.nextDouble() > randomness) {
          for (k <- selected) {
            constrain(cols(k), j, currentSolCols(k).contains(j))
            //println(k,"c",j,currentSolCols(k).contains(j))
          }
        }
        else if (!possibleCols.contains(j))
          for (k <- selected) {
            cols(k).excludes(j)
            //println(k,"c",j,false)
          }
      }
      for(i <- 0 until nRows) {
        for(k <- 0 until nsubmatrices)
          if(!selected.contains(k)) {
            constrain(lines(k), i, currentSolLines(k).contains(i))
            //println(k,"r",i,currentSolLines(k).contains(i))
          }
      }
      for(j <- 0 until nCols) {
        for(k <- 0 until nsubmatrices)
          if(!selected.contains(k)) {
            constrain(cols(k), j, currentSolCols(k).contains(j))
            //println(k,"c",j,currentSolCols(k).contains(j))
          }
      }
      cp.propagate()
    }
    randomness = updateRandomness(randomness, o1.completed)
    //println(o1)
  }

  def name: String = "RelaxKM with k="+relaxN+" with random " + randomness
}
