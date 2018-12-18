package search

import oscar.algo.reversible.{ReversibleDouble, ReversibleSparseSet}
import oscar.algo.search.{Branching, Decision}
import oscar.cp.Alternative
import oscar.cp.core.delta.DeltaSetVar
import oscar.cp.core.variables.CPSetVar
import utils.Status

class BestExpectationSetFast(rows: Array[CPSetVar],
                             cols: Array[CPSetVar],
                             matrix: Array[Array[Double]],
                             var takableBefore: Boolean = true) extends Branching {
  val s = rows(0).store
  val nRows = matrix.length
  val nCols = matrix(0).length
  val nSubMatrices = rows.length

  val taken = Array.fill(nRows, nCols)(false)

  val rowsDelta = rows.map(x => new DeltaSetVar(x,0))
  val colsDelta = cols.map(x => new DeltaSetVar(x,0))
  val required = new ReversibleSparseSet(s, 0, nRows*nCols)

  val cacheRowDelta = Array.fill(nRows)(0)
  val cacheColDelta = Array.fill(nCols)(0)
  val cacheRowDelta2 = Array.fill(nRows)(0)
  val cacheColDelta2 = Array.fill(nCols)(0)
  val cacheColDeltaSet = Array.fill(nCols)(false)

  val rowPossibleValue = Array.fill(nRows,nSubMatrices)(new ReversibleDouble(s, 0.0))
  val rowTakableValue = Array.fill(nRows,nSubMatrices)(new ReversibleDouble(s, 0.0))
  val colPossibleValue = Array.fill(nCols,nSubMatrices)(new ReversibleDouble(s, 0.0))
  val colTakableValue = Array.fill(nCols,nSubMatrices)(new ReversibleDouble(s, 0.0))

  for(i <- 0 until nRows) {
    for (j <- 0 until nCols) {
      if (matrix(i)(j) > 0) {
        for (k <- 0 until nSubMatrices) {
          rowPossibleValue(i)(k) += matrix(i)(j)
          colPossibleValue(j)(k) += matrix(i)(j)
        }
      }
    }
  }

  for(k <- 0 until nSubMatrices) {
    rowsDelta(k).update()
    colsDelta(k).update()
  }

  @inline
  private def require(i: Int, j: Int) = required.removeValue(i*nCols+j)

  @inline
  private def isAlreadyRequired(i: Int, j: Int): Boolean = !required.hasValue(i*nCols+j)

  @inline
  private def populateCacheColDeltaSet(nRemovedCols: Int, nRequiredCols: Int): Unit = {
    var i = 0
    while (i < nRemovedCols) {
      cacheColDeltaSet(cacheColDelta(i)) = true
      i += 1
    }
    i = 0
    while (i < nRequiredCols) {
      cacheColDeltaSet(cacheColDelta2(i)) = true
      i += 1
    }
  }

  def processDeltaLine(i: Int, j: Int, k: Int, newL: Status, oldC: Status): Boolean = {
    if(!isAlreadyRequired(i, j)) {
      if (oldC == Status.BOTTOM) {
        if (newL == Status.TRUE)
          colTakableValue(j)(k) += matrix(i)(j)
        else if (matrix(i)(j) > 0)
          colPossibleValue(j)(k) -= matrix(i)(j)
        false
      }
      else if (oldC == Status.TRUE)
        newL == Status.TRUE
      else
        false
    }
    else
      false
  }

  @inline
  def processDeltaCol(i: Int, j: Int, k: Int, oldL: Status, newC: Status): Boolean = {
    if(!isAlreadyRequired(i, j)) {
      if(oldL == Status.BOTTOM) {
        if(newC == Status.TRUE)
          rowTakableValue(i)(k) += matrix(i)(j)
        else if(matrix(i)(j) > 0)
          rowPossibleValue(i)(k) -= matrix(i)(j)
        false
      }
      else if(oldL == Status.TRUE)
        newC == Status.TRUE
      else
        false
    }
    else
      false
  }

  @inline
  def statusFromSet(set: CPSetVar, entry: Int): Status = {
    if(!set.isPossible(entry)) Status.FALSE
    else if(set.isRequired(entry)) Status.TRUE
    else Status.BOTTOM
  }

  private val newRequiredCellsRows = Array.fill(nRows*nCols)(0)
  private val newRequiredCellsCols = Array.fill(nRows*nCols)(0)

  def update(): Boolean = {
    var k = 0
    var bound = true
    while (k != nSubMatrices && bound) {
      bound = rows(k).isBound && cols(k).isBound
      k += 1
    }

    if(bound)
      return false

    var nNewRequiredCells = 0

    // O(dRequired * nSets + (nRows+nCols)*k + sum_k (dRows*nCols + dCols*nRows))

    k = 0
    var l = 0
    while(k != nSubMatrices) {
      val nRemovedRows = rowsDelta(k).deltaPossibleArray(cacheRowDelta)
      val nRemovedCols = colsDelta(k).deltaPossibleArray(cacheColDelta)
      val nNewRequiredRows = rowsDelta(k).deltaRequiredArray(cacheRowDelta2)
      val nNewRequiredCols = colsDelta(k).deltaRequiredArray(cacheColDelta2)
      populateCacheColDeltaSet(nRemovedCols, nNewRequiredCols)

      l = 0
      while(l != nRemovedRows) {
        val i = cacheRowDelta(l)
        var j = 0
        while(j != nCols) {
          if (processDeltaLine(i, j, k, Status.FALSE, if (cacheColDeltaSet(j)) Status.BOTTOM else statusFromSet(cols(k), j))) {
            newRequiredCellsRows(nNewRequiredCells) = i
            newRequiredCellsCols(nNewRequiredCells) = j
            nNewRequiredCells += 1
          }
          j += 1
        }
        l += 1
      }

      l = 0
      while(l != nNewRequiredRows) {
        val i = cacheRowDelta2(l)
        var j = 0
        while(j != nCols) {
          if (processDeltaLine(i, j, k, Status.TRUE, if (cacheColDeltaSet(j)) Status.BOTTOM else statusFromSet(cols(k), j))) {
            newRequiredCellsRows(nNewRequiredCells) = i
            newRequiredCellsCols(nNewRequiredCells) = j
            nNewRequiredCells += 1
          }
          j += 1
        }
        l += 1
      }

      l = 0
      while(l != nRemovedCols) {
        val j = cacheColDelta(l)
        var i = 0
        cacheColDeltaSet(j) = false
        while(i != nRows) {
          if (processDeltaCol(i, j, k, statusFromSet(rows(k), i), Status.FALSE)) {
            newRequiredCellsRows(nNewRequiredCells) = i
            newRequiredCellsCols(nNewRequiredCells) = j
            nNewRequiredCells += 1
          }
          i += 1
        }
        l += 1
      }

      l = 0
      while(l != nNewRequiredCols) {
        val j = cacheColDelta2(l)
        var i = 0
        cacheColDeltaSet(j) = false
        while(i != nRows) {
          if (processDeltaCol(i, j, k, statusFromSet(rows(k), i), Status.TRUE)) {
            newRequiredCellsRows(nNewRequiredCells) = i
            newRequiredCellsCols(nNewRequiredCells) = j
            nNewRequiredCells += 1
          }
          i += 1
        }
        l += 1
      }

      //to do last
      rowsDelta(k).update()
      colsDelta(k).update()
      k += 1
    }

    l = 0
    while(l != nNewRequiredCells) {
      val i = newRequiredCellsRows(l)
      val j = newRequiredCellsCols(l)
      require(i, j)

      k = 0
      while(k != nSubMatrices) {
        val rowStatus = statusFromSet(rows(k), i)
        val colStatus = statusFromSet(cols(k), j)
        if (rowStatus == Status.BOTTOM && colStatus == Status.BOTTOM) { //A to B
          if (matrix(i)(j) > 0) {
            rowPossibleValue(i)(k) -= matrix(i)(j)
            colPossibleValue(j)(k) -= matrix(i)(j)
          }
        }
        else if (rowStatus == Status.TRUE && colStatus == Status.BOTTOM) { //D to C
          if (matrix(i)(j) > 0)
            colPossibleValue(j)(k) -= matrix(i)(j)
          colTakableValue(j)(k) -= matrix(i)(j)
        }
        else if (rowStatus == Status.BOTTOM && colStatus == Status.TRUE) { //E to F
          if (matrix(i)(j) > 0)
            rowPossibleValue(i)(k) -= matrix(i)(j)
          rowTakableValue(i)(k) -= matrix(i)(j)
        }
        k += 1
      }
      l += 1
    }

    true
  }

  def select(): Seq[Alternative] = {
    var bestK: Int = -1
    var bestIdx: Int = -1
    var bestIsRow: Boolean = false
    var bestTakable = Double.MinValue
    var bestPossible = Double.MinValue

    var k = 0
    while(k != nSubMatrices) {
      var i = 0
      while(i != nRows) {
        if(rows(k).isPossible(i) && !rows(k).isRequired(i)) {
          if(checkIfBetter(bestTakable, bestPossible, rowTakableValue(i)(k).value, rowPossibleValue(i)(k).value, k)) {
            //if (bestTakable < rowTakableValue(i)(k).value || (bestTakable == rowTakableValue(i)(k).value && bestPossible < rowPossibleValue(i)(k).value)) {
            bestTakable = rowTakableValue(i)(k).value
            bestPossible = rowPossibleValue(i)(k).value
            bestK = k
            bestIdx = i
            bestIsRow = true
          }
        }
        i += 1
      }
      var j = 0
      while(j != nCols) {
        if(cols(k).isPossible(j) && !cols(k).isRequired(j)) {
          if(checkIfBetter(bestTakable, bestPossible, colTakableValue(j)(k).value, colPossibleValue(j)(k).value, k)) {
            //if (bestTakable < colTakableValue(j)(k).value || (bestTakable == colTakableValue(j)(k).value && bestPossible < colPossibleValue(j)(k).value)) {
            bestTakable = colTakableValue(j)(k).value
            bestPossible = colPossibleValue(j)(k).value
            bestK = k
            bestIdx = j
            bestIsRow = false
          }
        }
        j += 1
      }
      k += 1
    }

    if(bestK != -1) {
      val v = if(bestIsRow) rows(bestK) else cols(bestK)
      //println((if(bestIsRow) "row" else "col") + " " + bestK + " " + bestIdx + " " + bestTakable + " " + bestPossible)
      List(
        Decision {v.context.requires(v, bestIdx)},
        Decision {v.context.excludes(v, bestIdx)}
      )
    }
    else
      List()
  }

  def alternatives(): Seq[Alternative] = {
    if(update())
      select()
    else
      List()
  }

  @inline
  private def checkIfBetter(bestTak: Double, bestPos: Double, newTak: Double, newPos: Double, newK: Int) = {
    if(takableBefore)
      bestTak < newTak || (bestTak == newTak && bestPos < newPos)// || (bestTak == newTak && bestPos == newPos)
    else
      bestPos < newPos || (bestPos == newPos && bestTak < newTak)
    //bestPos < newPos || (bestPos == newPos && bestTak < newTak) || (bestTak == newTak && bestPos == newPos)
    //bestPos + bestTak < newPos + newTak
  }
}
