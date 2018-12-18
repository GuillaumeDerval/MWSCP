package contraints

import oscar.algo.reversible.{ReversibleDouble, ReversibleInt}
import oscar.cp.core.variables.{CPSetVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}
import utils.ReversibleSparseSubsetSupport

/**
  * Computes an upper bound for the KMSS problem.
  *
  * Takes the sum of taken cells, and add all the available positive cells.
  *
  * Complexity is O(n) per variable binding.
  *
  * @param lines
  * @param cols
  * @param matrix
  * @param value
  */
class DominanceRuleSetCorrectV2(nRows: Int, nCols: Int,
                                rows: Array[CPSetVar],
                                cols: Array[CPSetVar],
                                matrix: Array[Array[Double]],
                                allowDirectSolution: Boolean = true,
                                EPSILON: Double = 1e-8) extends Constraint(rows(0).store) {

  val nSets = rows.length

  val supports = Array.fill(nRows, nCols)(new ReversibleInt(s, nSets))

  val rowsDelta = rows.map(x => x.delta(this))
  val colsDelta = cols.map(x => x.delta(this))

  override def associatedVars(): Iterable[CPVar] = null

  override def setup(l: CPPropagStrength): Unit = {
    for(k <- 0 until nSets) {
      rows(k).callPropagateWhenDomainChanges(this)
      cols(k).callPropagateWhenDomainChanges(this)
    }


    recompute()
  }

  val bestRowValue = Array.fill(nSets,nRows)(new ReversibleDouble(s, 0.0))
  val worstRowValue = Array.fill(nSets,nRows)(new ReversibleDouble(s, 0.0))
  val justUpdated = new java.util.HashSet[(Int, Int, Int)]()

  private def deltaRemove(i: Int, j: Int, k: Int): Unit = {
    if(matrix(i)(j) < 0)
      worstRowValue(k)(i).value = worstRowValue(k)(i).value - matrix(i)(j)
    else
      bestRowValue(k)(i).value =  bestRowValue(k)(i).value - matrix(i)(j)
  }

  private def decreaseSupport(i: Int, j: Int): Unit = {
    supports(i)(j) -= 1
    if(supports(i)(j).value == 1) {
      var k = 0
      while(k != nSets) {
        if(rows(k).isPossible(i) && cols(k).isRequired(j)) {
          //if(justUpdated.contains((i,j,k)))
          //  throw new RuntimeException("Should never happen")
          justUpdated.add((i,j,k))
          if (matrix(i)(j) > 0.0)
            worstRowValue(k)(i) += matrix(i)(j)
          else
            bestRowValue(k)(i) += matrix(i)(j)
        }
        k += 1
      }
    }
  }

  private def deltaRequire(i: Int, j: Int, k: Int): Unit = {
    if(!justUpdated.contains((i,j,k))) {
      if(supports(i)(j).value == 1) {
        justUpdated.add((i,j,k))
        if (matrix(i)(j) > 0.0)
          worstRowValue(k)(i) += matrix(i)(j)
        else
          bestRowValue(k)(i) += matrix(i)(j)
      }
    }
  }

  val cacheRowDeltaRemoved = Array.fill(nRows)(0)
  val cacheColDeltaRemoved = Array.fill(nCols)(0)
  val cacheColDeltaSetRemoved = Array.fill(nCols)(false)
  val cacheColDeltaRequired = Array.fill(nCols)(0)

  @inline
  private def populateCacheColDeltaSet(set: Array[Boolean], array: Array[Int], size: Int): Unit = {
    var i = 0
    while (i < size) {
      set(array(i)) = true
      i += 1
    }
  }

  def printDelta(array: Array[Int], size: Int, name: String): Unit = {
    if(size > 0) {
      print(name)
      for(i <- 0 until size)
        print(" " + array(i))
      println()
    }
  }

  override def propagate(): Unit = {
    super.propagate()
    justUpdated.clear()

    //recompute is O(nSets*nRows*nCols)
    //update is O(delta_rows_removed*nCols*nSets +
    //            delta_cols_removed*nRows*nSets +
    //            delta_cols_required*nRows)
    // let's compute who is the best to start

    var k = 0
    var deltaRowsRemoved = 0
    var deltaColsRemoved = 0
    var nUnbound = 0
    while (k != nSets) {
      deltaRowsRemoved += rowsDelta(k).deltaPossibleSize()
      deltaColsRemoved += colsDelta(k).deltaPossibleSize()
      if(!rows(k).isBound)
        nUnbound += 1
      if(!cols(k).isBound)
        nUnbound += 1
      k += 1
    }

    if(nUnbound == 0 && allowDirectSolution) {
      // do nothing
    }
    else {
      if (deltaRowsRemoved < nRows && deltaColsRemoved < nCols)
        update()
      else
        recompute()

      prune()

      /*TEST += 1
      if(TEST % 100 == 0)
        recompute(true)*/
    }
  }

  val pruneRowCache = Array.fill(nRows)(0)
  private def prune(): Unit = {
    var k = 0
    while(k != nSets) {
      var idx = 0

      val nPossible = rows(k).fillArrayPossibleAndNotRequired(pruneRowCache)
      while(idx != nPossible) {
        val i = pruneRowCache(idx)
        if(bestRowValue(k)(i).value < -EPSILON) {
          rows(k).excludes(i)
          var j = 0
          while(j != nCols) {
            if (cols(k).isPossible(j)) {
              deltaRemove(i, j, k)
              decreaseSupport(i,j)
            }

            j += 1
          }
        }
        else if(worstRowValue(k)(i).value > EPSILON)
          rows(k).requires(i)
        idx += 1
      }
      k += 1
    }
  }

  private def update(): Unit = {
    var k = 0
    var l = 0

    //println("---")
    while(k != nSets) {
      val nRemovedRows = rowsDelta(k).deltaPossibleArray(cacheRowDeltaRemoved)
      val nRemovedCols = colsDelta(k).deltaPossibleArray(cacheColDeltaRemoved)
      populateCacheColDeltaSet(cacheColDeltaSetRemoved, cacheColDeltaRemoved, nRemovedCols)

      val nNewRequiredCols = colsDelta(k).deltaRequiredArray(cacheColDeltaRequired)

      l = 0
      while(l < nRemovedRows) {
        val i = cacheRowDeltaRemoved(l)
        var j = 0
        while(j != nCols) {
          if(cacheColDeltaSetRemoved(j) || cols(k).isPossible(j)) {
            deltaRemove(i, j, k)
            decreaseSupport(i, j)
          }
          j += 1
        }
        l += 1
      }
      l = 0
      while(l < nRemovedCols) {
        var i = 0
        val j = cacheColDeltaRemoved(l)
        cacheColDeltaSetRemoved(j) = false // restore cache state
        while(i != nRows) {
          if(rows(k).isPossible(i)) {
            deltaRemove(i, j, k)
            decreaseSupport(i,j)
          }
          i += 1
        }
        l += 1
      }

      l = 0
      while(l < nNewRequiredCols) {
        var i = 0
        val j = cacheColDeltaRequired(l)
        while(i != nRows) {
          deltaRequire(i, j, k)
          i += 1
        }
        l += 1
      }

      k += 1
    }

    //recompute(true)

    /*TEST += 1
    if(TEST % 100 == 0)
    recompute(true)*/

  }
  var TEST = 0

  private def recomputeSlow(check: Boolean = false): Unit = {


    //supports
    for(i <- 0 until nRows) {
      for(j <- 0 until nCols) {
        var nSup = 0
        for(k <- 0 until nSets)
          if(rows(k).isPossible(i) && cols(k).isPossible(j))
            nSup += 1
        if(check && supports(i)(j).value != nSup)
          throw new RuntimeException("Invalid support computation!")
        supports(i)(j).value = nSup
      }
    }

    //bestRowValue
    //worstRowValue
    for(i <- 0 until nRows) {
      for(k <- 0 until nSets) {
        if(rows(k).isPossible(i)) {
          // compute worst case: we take
          // - everything required
          // - everything possible and negative
          // and we remove
          // - everything possible (for other sets) and positive
          val p = cols(k).possibleSet().filter(j => matrix(i)(j) < 0.0 || (cols(k).isRequired(j) && supports(i)(j).value == 1))
            //(matrix(i)(j) < 0.0 || cols(k).isRequired(j)) && !(matrix(i)(j) >= 0.0 && supports(i)(j).value > 1))
          val wp = p.foldLeft(0.0){case (c, j) => c + matrix(i)(j)}

          // compute best case: we take
          // - everything required
          // - everything possible and positive
          // and we remove
          // - everything possible (for other sets) and negative
          val n = cols(k).possibleSet().filter(j => matrix(i)(j) > 0.0 || (cols(k).isRequired(j) && supports(i)(j).value == 1))
            //(matrix(i)(j) >= 0.0 || cols(k).isRequired(j)) && !(matrix(i)(j) < 0.0 && supports(i)(j).value > 1))
          val wn = n.foldLeft(0.0){case (c, j) => c + matrix(i)(j)}

          if(check && !almostEqual(bestRowValue(k)(i).value, wn))
            throw new RuntimeException("Invalid best value computation!")
          if(check && !almostEqual(worstRowValue(k)(i).value,wp))
            throw new RuntimeException("Invalid worst value computation!")
          bestRowValue(k)(i).value = wn
          worstRowValue(k)(i).value = wp
        }
      }
    }
  }

  val recomputeRowCache = Array.fill(nRows)(0)
  val recomputeColCache = Array.fill(nCols)(0)

  private def recompute(check: Boolean = false): Unit = {
    //supports
    var i = 0
    while(i != nRows) {
      var j = 0
      while(j != nCols) {
        var nSup = 0
        var k = 0
        while (k != nSets) {
          if (rows(k).isPossible(i) && cols(k).isPossible(j))
            nSup += 1
          k += 1
        }
        if(check && supports(i)(j).value != nSup)
          throw new RuntimeException("Invalid support computation!")
        supports(i)(j).value = nSup
        j += 1
      }
      i += 1
    }

    //bestRowValue
    //worstRowValue
    var k = 0
    while (k != nSets) {
      var idxI = 0
      val nPossibleI = rows(k).fillArrayPossible(recomputeRowCache)
      while (idxI != nPossibleI) {
        val i = recomputeRowCache(idxI)

        val nPossibleJ = cols(k).fillArrayPossible(recomputeColCache)
        var idxJ = 0
        var wp = 0.0
        var wn = 0.0
        while (idxJ != nPossibleJ) {
          val j = recomputeColCache(idxJ)
          // compute worst case: we take
          // - everything required
          // - everything possible and negative
          // and we remove
          // - everything possible (for other sets) and positive
          if( matrix(i)(j) < 0.0 || (cols(k).isRequired(j) && supports(i)(j).value == 1))
            wp += matrix(i)(j)
          // compute best case: we take
          // - everything required
          // - everything possible and positive
          // and we remove
          // - everything possible (for other sets) and negative
          if(matrix(i)(j) > 0.0 || (cols(k).isRequired(j) && supports(i)(j).value == 1))
            wn += matrix(i)(j)
          idxJ += 1
        }

        if(check && !almostEqual(bestRowValue(k)(i).value, wn))
          throw new RuntimeException("Invalid best value computation!")
        if(check && !almostEqual(worstRowValue(k)(i).value,wp))
          throw new RuntimeException("Invalid worst value computation!")
        bestRowValue(k)(i).value = wn
        worstRowValue(k)(i).value = wp

        idxI += 1
      }
      k += 1
    }
    /*i = 0
    while(i != nRows) {
      var k = 0
      while(k != nSets) {
        if(rows(k).isPossible(i)) {

          val nPossible = cols(k).fillArrayPossible(recomputeColCache)
          var idx = 0
          var wp = 0.0
          var wn = 0.0
          while (idx != nPossible) {
            val j = recomputeColCache(idx)
            // compute worst case: we take
            // - everything required
            // - everything possible and negative
            // and we remove
            // - everything possible (for other sets) and positive
            if( matrix(i)(j) < 0.0 || (cols(k).isRequired(j) && supports(i)(j).value == 1))
              wp += matrix(i)(j)
            // compute best case: we take
            // - everything required
            // - everything possible and positive
            // and we remove
            // - everything possible (for other sets) and negative
            if(matrix(i)(j) > 0.0 || (cols(k).isRequired(j) && supports(i)(j).value == 1))
              wn += matrix(i)(j)
            idx += 1
          }

          if(check && !almostEqual(bestRowValue(k)(i).value, wn))
            throw new RuntimeException("Invalid best value computation!")
          if(check && !almostEqual(worstRowValue(k)(i).value,wp))
            throw new RuntimeException("Invalid worst value computation!")
          bestRowValue(k)(i).value = wn
          worstRowValue(k)(i).value = wp
        }
        k += 1
      }
      i += 1
    }*/
  }

  def almostEqual(a: Double, b: Double): Boolean = (a - b).abs < 1e-8
}
