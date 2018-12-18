package models

import java.util.concurrent.LinkedBlockingQueue

import contraints.{DominanceRuleSetCorrectV2, PositiveTakableBoundFastSet}
import models.lnsoperators._
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPSetVar
import search._
import utils.{DoubleVar, MaximiseDouble, Solution, Solver}

import scala.collection.immutable.ListMap
import scala.util.Random

class GenericLNSCut(useDominance: Boolean = true) extends Solver {

  def solve(matrix: Array[Array[Double]], k: Int, queue: LinkedBlockingQueue[Solution]) = {
    val nRows = matrix.length
    val nCols = matrix(0).length

    // -------------- CP Model --------------
    implicit val cp = CPSolver()

    val nsubmatrices = k

    val lines = Array.fill(nsubmatrices)(new CPSetVar(cp, 0, nRows - 1))
    val cols = Array.fill(nsubmatrices)(new CPSetVar(cp, 0, nCols - 1))

    val time = System.currentTimeMillis()

    val doubleObj = new DoubleVar(0, Double.MaxValue)
    val doubleMax = new MaximiseDouble(doubleObj)

    val allVars: Array[CPSetVar] = lines ++ cols

    val baseSearch = new BestExpectationSetFast(lines, cols, matrix)

    cp.search(new ImmediateSolutionSet(allVars, doubleObj, doubleMax, baseSearch))

    cp.postCut(doubleMax)

    val c1 = new PositiveTakableBoundFastSet(lines, cols, matrix, doubleObj)
    cp.post(c1)

    if(useDominance) {
      val c2 = new DominanceRuleSetCorrectV2(nRows, nCols, lines, cols, matrix)
      cp.post(c2)

      val c3 = new DominanceRuleSetCorrectV2(nCols, nRows, cols, lines, matrix.transpose)
      cp.post(c3)
    }

    val currentSolLines = Array.fill(nsubmatrices)(Set[Int]())
    val currentSolCols = Array.fill(nsubmatrices)(Set[Int]())

    var curOp: Operator = null

    var mode = 0
    val random = new Random(11)

    val operators: Map[Operator, Double] = ListMap(
      //stdOp -> 1.0,
      new RelaxMatrix(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random) -> 1.0,
      new RelaxKSubmatrices(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random, nsubmatrices).asInstanceOf[Operator] -> 1.0,
      new RelaxKSubmatricesM(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random, nsubmatrices).asInstanceOf[Operator] -> 1.0
      //new OnlyPossibleLines(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random) -> 1.0
    ) ++
      (2 until nsubmatrices).map(i => new RelaxKSubmatrices(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random, i) -> 2.0/(nsubmatrices-2)) ++
      (2 until nsubmatrices).map(i => new RelaxKSubmatricesM(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random, i) -> 2.0/(nsubmatrices-2))

    cp.onSolution {
      queue.add(Solution(doubleObj.value, lines.map(_.requiredSet().toArray), cols.map(_.requiredSet().toArray), System.currentTimeMillis() - time, curOp.name))
      doubleMax.updateLB(doubleObj.value)
      for (k <- 0 until nsubmatrices) {
        currentSolLines(k) = lines(k).requiredSet()
        currentSolCols(k) = cols(k).requiredSet()
      }
      //for(o <- operators.keys) println(o.name)
    }

    val stdOp = new Std(cp, nsubmatrices, nRows, nCols, lines, cols, currentSolLines, currentSolCols, random)
    curOp = stdOp
    stdOp.run()


    val selector = new OperatorSelector(operators, random)

    while (true) {
      baseSearch.takableBefore = random.nextBoolean()
      curOp = selector.select()
      curOp.run()
    }


    queue.add(Solution(Double.NaN, null, null, -1, null))
  }
}
