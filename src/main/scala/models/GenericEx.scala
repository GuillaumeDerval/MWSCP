package models

import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}

import contraints.{DominanceRuleSetCorrectV2, LexLeqSet, PositiveTakableBoundFastSet}
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPSetVar
import parsers.ParseTSV
import search._
import utils.{DoubleVar, MaximiseDouble, Solution, Solver}

import scala.util.Random

object GenericEx extends Solver {

  def solve(matrix: Array[Array[Double]], k: Int, queue: LinkedBlockingQueue[Solution]) = {

    val nRows = matrix.length
    val nCols = matrix(0).length

    // -------------- CP Model --------------
    implicit val cp = CPSolver()

    val nsubmatrices = k

    val lines = Array.fill(nsubmatrices)(new CPSetVar(cp, 0, nRows-1))
    val cols = Array.fill(nsubmatrices)(new CPSetVar(cp, 0, nCols-1))

    val time = System.currentTimeMillis()

    val doubleObj = new DoubleVar(0, Double.MaxValue)
    val doubleMax = new MaximiseDouble(doubleObj)

    val allVars: Array[CPSetVar] = lines ++ cols

    cp.search(new ImmediateSolutionSet(allVars, doubleObj, doubleMax, new BestExpectationSetFast(lines, cols, matrix)))

    for(k <- 0 until nsubmatrices - 1)
      cp.post(new LexLeqSet(lines(k), lines(k+1)))

    cp.postCut(doubleMax)

    val c1 = new PositiveTakableBoundFastSet(lines, cols, matrix, doubleObj)
    cp.post(c1)

    val c2 = new DominanceRuleSetCorrectV2(nRows, nCols, lines, cols, matrix)
    cp.post(c2)

    val c3 = new DominanceRuleSetCorrectV2(nCols, nRows, cols, lines, matrix.transpose)
    cp.post(c3)

    cp.onSolution {
      queue.add(Solution(doubleObj.value, lines.map(_.requiredSet().toArray), cols.map(_.requiredSet().toArray), System.currentTimeMillis() - time, null))
      /*println("-----------------------------------------------------------------------")
      println("New best bound: " + doubleObj.value + "\tTime: " + (System.currentTimeMillis() - time))
      println("Rows [" + lines.map(_.requiredSet().mkString(",")).mkString("],\t[") + "]")
      println("Cols [" + cols.map(_.requiredSet().mkString(",")).mkString("],\t[") + "]")*/
      doubleMax.updateLB(doubleObj.value)
    }

    val o1 = cp.start()
    //println(o1)

    queue.add(Solution(Double.NaN, null, null, -1, null))
  }
}
