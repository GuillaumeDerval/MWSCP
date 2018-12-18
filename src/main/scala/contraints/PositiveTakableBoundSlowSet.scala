package contraints

import oscar.cp.CPBoolVar
import oscar.cp.core.variables.{CPSetVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}
import utils.DoubleVar

/**
  * Computes an upper bound for the MSMs problem.
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
class PositiveTakableBoundSlowSet(lines: Array[CPSetVar],
                                  cols: Array[CPSetVar],
                                  matrix: Array[Array[Double]],
                                  value: DoubleVar) extends Constraint(cols(0).store) {

  val nMatrixRows = matrix.length
  val nMatrixCols = matrix(0).length
  val nSets = cols.length

  override def associatedVars(): Iterable[CPVar] = null//(lines.flatten ++ cols.flatten).toArray[CPVar]

  override def setup(l: CPPropagStrength): Unit = {
    lines.foreach(_.callPropagateWhenDomainChanges(this))
    cols.foreach(_.callPropagateWhenDomainChanges(this))
    propagate()
  }

  override def propagate(): Unit = {
    super.propagate()

    var ub = 0.0
    var lb = 0.0
    for(i <- 0 until nMatrixRows) {
      for(j <- 0 until nMatrixCols) {
        var used = false
        var possible = false
        for(k <- 0 until nSets) {
          if(lines(k).isPossible(i) && cols(k).isPossible(j)) {
            possible = true
            if(lines(k).isRequired(i) && cols(k).isRequired(j))
              used = true
          }
        }
        if(used) {
          ub += matrix(i)(j)
          lb += matrix(i)(j)
        }
        else if(possible) {
          if(matrix(i)(j) >= 0)
            ub += matrix(i)(j)
          else
            lb += matrix(i)(j)
        }

      }
    }

    value.updateMin(lb)
    value.updateMax(ub)
  }
}
