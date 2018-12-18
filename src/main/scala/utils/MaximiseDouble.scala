package utils

import oscar.cp.core.variables.CPVar
import oscar.cp.core.{CPPropagStrength, Constraint}

class MaximiseDouble(obj: DoubleVar, epsilon: Double = 1e-6) extends Constraint(obj.cp) {
  override def associatedVars(): Iterable[CPVar] = Array[CPVar]()

  var best = Double.MinValue
  private var lb = java.lang.Math.nextAfter(best, Double.MaxValue)

  override def setup(l: CPPropagStrength): Unit = {

  }

  override def propagate(): Unit = {
    obj.updateMin(lb + epsilon)
  }

  def updateLB(newLB: Double): Unit = {
    best = best.max(newLB)
    lb = best
  }
}
