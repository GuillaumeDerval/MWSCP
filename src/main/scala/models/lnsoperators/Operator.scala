package models.lnsoperators

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPSetVar

import scala.util.Random

abstract class Operator(cp: CPSolver, nsubmatrices: Int,
                        nRows: Int, nCols: Int,
                        lines: Array[CPSetVar], cols: Array[CPSetVar],
                        currentSolLines: Array[Set[Int]], currentSolCols: Array[Set[Int]],
                        random: Random) {

  @inline
  def constrainRandomly(set: CPSetVar, value: Int, inSol: Boolean, randomness: Double): Unit = {
    if (random.nextDouble() > randomness) {
      if (inSol)
        set.requires(value)
      else
        set.excludes(value)
    }
  }

  @inline
  def constrain(set: CPSetVar, value: Int, inSol: Boolean): Unit = {
    if(inSol)
      set.requires(value)
    else
      set.excludes(value)
  }

  @inline
  def updateRandomness(randomness: Double, complete: Boolean): Double = {
    val v = Math.max(Math.min(1.0-randomness, randomness), 0.001) * 0.05
    val inc = if (complete) v else -v
    Math.min(Math.max(0.01, randomness+inc), 0.99)
  }

  def run(): Unit
  def name: String
}
