package contraints

import oscar.algo.reversible.{ReversibleDouble, ReversibleInt, ReversibleSparseSet}
import oscar.cp.core.variables.{CPSetVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}
import utils.{DoubleVar, ReversibleSparseSubsetSupport}

/**
  * Computes an upper bound for the MSMs problem.
  *
  * Takes the sum of taken cells, and add all the available positive cells.
  *
  * @param lines
  * @param cols
  * @param matrix
  * @param value
  */
class PositiveTakableBoundFastSet(rows: Array[CPSetVar],
                                  cols: Array[CPSetVar],
                                  matrix: Array[Array[Double]],
                                  value: DoubleVar) extends Constraint(cols(0).store) {

  val nRows = matrix.length
  val nCols = matrix(0).length
  val nSets = cols.length

  val lowerBound = new ReversibleDouble(s, Double.MinValue)
  val upperBound = new ReversibleDouble(s, Double.MaxValue)
  val curBound = new ReversibleDouble(s, Double.MaxValue)

  val rowsDelta = rows.map(x => x.delta(this))
  val colsDelta = cols.map(x => x.delta(this))
  val supports = Array.fill(nRows)(new ReversibleSparseSubsetSupport(s, 0, nCols-1, nSets))
  val required = new ReversibleSparseSet(s, 0, nRows*nCols)
  val errorCount = new ReversibleInt(s, 0)

  override def associatedVars(): Iterable[CPVar] = null//(lines.flatten ++ cols.flatten).toArray[CPVar]

  override def setup(l: CPPropagStrength): Unit = {
    rows.foreach(_.callPropagateWhenDomainChanges(this))
    cols.foreach(_.callPropagateWhenDomainChanges(this))

    recompute()
    propagate()
  }

  @inline
  private def decreaseSupport(i: Int, j: Int): Unit = {
    supports(i).decreaseSupport(j)
    if(!supports(i).isPossible(j)) {
      if(matrix(i)(j) < 0)
        lowerBound.value = lowerBound.value - matrix(i)(j)
      else
        upperBound.value = upperBound.value - matrix(i)(j)
    }
  }

  @inline
  private def isAlreadyRequired(i: Int, j: Int): Boolean = !required.hasValue(i*nCols+j)

  @inline
  private def deltaRequire(i: Int, j: Int) = {
    required.removeValue(i*nCols+j)
    if(matrix(i)(j) > 0)
      lowerBound.value = lowerBound.value + matrix(i)(j)
    else
      upperBound.value = upperBound.value + matrix(i)(j)
    curBound.value = curBound.value + matrix(i)(j)
  }

  val cacheRowDelta = Array.fill(nRows)(0)
  val cacheColDelta = Array.fill(nCols)(0)
  val cacheColDeltaSet = Array.fill(nCols)(false)

  val updateThreshold = nRows*nCols*nSets / 5

  @inline
  private def populateCacheColDeltaSet(nRemovedCols: Int): Unit = {
    var i = 0
    while (i < nRemovedCols) {
      cacheColDeltaSet(cacheColDelta(i)) = true
      i += 1
    }
  }

  override def propagate(): Unit = {
    super.propagate()

    var k = 0

    var nColsBound = 0
    var nRowsBound = 0
    var deltaPossibleRows = 0
    var deltaPossibleCols = 0
    var deltaRequiredRows = 0
    var deltaRequiredCols = 0

    while (k != nSets) {
      deltaPossibleRows += rowsDelta(k).deltaPossibleSize()
      deltaPossibleCols += colsDelta(k).deltaPossibleSize()
      deltaRequiredRows += rowsDelta(k).deltaRequiredSize()
      deltaRequiredCols += colsDelta(k).deltaRequiredSize()
      if(rows(k).isBound)
        nRowsBound += 1
      if(cols(k).isBound)
        nColsBound += 1
      k += 1
    }

    val nOp = (deltaPossibleRows + deltaRequiredRows) * nCols + (deltaPossibleCols + deltaRequiredCols) * nRows

    if(nColsBound == nSets && nRowsBound == nSets && deltaRequiredCols + deltaRequiredRows == 0)
      setCB()
    else if(nOp < updateThreshold)
      update()
    else
      recompute()
  }

  private def setCB(): Unit = {
    lowerBound.value = curBound.value
    upperBound.value = curBound.value
    value.updateMin(lowerBound.value)
    value.updateMax(upperBound.value)
  }

  private def update(): Unit = {
    //sum_k (dpr_k * nCols + dpc_k * nRows + drr_k * nCols + drc_k * nRows)
    //sum_k (dpr_k + drr_k) * nCols + sum_k (dpc_k + drc_k))* nRows
    var k = 0
    var l = 0

    while(k != nSets) {
      val nRemovedRows = rowsDelta(k).deltaPossibleArray(cacheRowDelta)
      val nRemovedCols = colsDelta(k).deltaPossibleArray(cacheColDelta)
      populateCacheColDeltaSet(nRemovedCols)
      l = 0
      while(l < nRemovedRows) {
        val i = cacheRowDelta(l)
        var j = 0
        while(j != nCols) {
          if(cacheColDeltaSet(j) || cols(k).isPossible(j))
            decreaseSupport(i,j)
          j += 1
        }
        l += 1
      }
      l = 0
      while(l < nRemovedCols) {
        var i = 0
        val j = cacheColDelta(l)
        cacheColDeltaSet(j) = false // restore cache state
        while(i != nRows) {
          if(rows(k).isPossible(i))
            decreaseSupport(i,j)
          i += 1
        }
        l += 1
      }

      // manage required things
      val nNewRequiredRows = rowsDelta(k).deltaRequiredArray(cacheRowDelta)
      val nNewRequiredCols = colsDelta(k).deltaRequiredArray(cacheColDelta)
      l = 0
      while(l < nNewRequiredRows) {
        val i = cacheRowDelta(l)
        var j = 0
        while (j != nCols) {
          if (cols(k).isRequired(j) && !isAlreadyRequired(i, j))
            deltaRequire(i,j)
          j += 1
        }
        l += 1
      }
      l = 0
      while(l < nNewRequiredCols) {
        var i = 0
        val j = cacheColDelta(l)
        while(i != nRows) {
          if (rows(k).isRequired(i) && !isAlreadyRequired(i, j))
            deltaRequire(i, j)
          i += 1
        }
        l += 1
      }

      k += 1
    }


    value.updateMin(lowerBound.value)
    value.updateMax(upperBound.value)
    errorCount += 1
    if(errorCount.value > 1000)
      checkCorrectness()
      //recompute() //correct computation errors
  }

  private def checkCorrectness(): Unit = {
    val cur = curBound.value
    val low = lowerBound.value
    val upp = upperBound.value
    recompute()
    if(!almostEqual(cur, curBound.value))
      println("ERROR CUR " + cur + " " + curBound.value)
    if(!almostEqual(low, lowerBound.value))
      println("ERROR LOW " + low + " " + lowerBound.value)
    if(!almostEqual(upp, upperBound.value))
      println("ERROR UPP " + upp + " " + upperBound.value)
  }

  val recomputeSupportCache = Array.fill(nRows, nCols)(0)
  private def recompute(): Unit = {
    //complexity nRows*nCols*nSets
    var ub = 0.0
    var lb = 0.0
    var cb = 0.0

    var i = 0
    while(i != nRows) {
      var j = 0
      while(j != nCols) {
        recomputeSupportCache(i)(j) = 0
        j += 1
      }
      i += 1
    }

    i = 0
    while(i != nRows) {
      var j = 0

      while(j != nCols) {
        var used = false
        var possible = false
        var k = 0
        while(k != nSets) {
          if(rows(k).isPossible(i) && cols(k).isPossible(j)) {
            possible = true
            if(rows(k).isRequired(i) && cols(k).isRequired(j))
              used = true
            recomputeSupportCache(i)(j) += 1
          }
          k += 1
        }
        if(used) {
          if(!isAlreadyRequired(i,j))
            required.removeValue(i*nCols+j)
          ub += matrix(i)(j)
          lb += matrix(i)(j)
          cb += matrix(i)(j)
        }
        else if(possible) {
          if(matrix(i)(j) >= 0)
            ub += matrix(i)(j)
          else
            lb += matrix(i)(j)
        }
        j += 1
      }
      i += 1
    }

    i = 0
    while(i != nRows) {
      var j = 0
      while(j != nCols) {
        supports(i).setSupport(j, recomputeSupportCache(i)(j))
        j += 1
      }
      i += 1
    }

    //if(!almostEqual(lowerBound.value,lb) || !almostEqual(upperBound.value,ub))
    //  println(lowerBound.value, lb, upperBound.value, ub)
    lowerBound.setValue(lb)
    upperBound.setValue(ub)
    curBound.setValue(cb)
    value.updateMin(lowerBound.value)
    value.updateMax(upperBound.value)
    errorCount.setValue(0)
  }

  def almostEqual(a: Double, b: Double): Boolean = (a - b).abs < 1e-8
}
