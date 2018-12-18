package utils

import oscar.cp.core.variables.{CPIntVar, CPVar}

class MaximiseDoubleOrCard(obj: DoubleVar, card: CPIntVar, epsilon: Double = 1e-6) extends MaximiseDouble(obj, epsilon) {
  override def associatedVars(): Iterable[CPVar] = Array[CPVar]()

  var bestObj = Double.MinValue
  private var lbObj = java.lang.Math.nextAfter(bestObj, Double.MaxValue)

  var bestCard = 0
  private var lbCard = 0

  var mode = true

  def setMode(obj: Boolean) = {
    mode = obj
  }

  override def propagate(): Unit = {
    if(mode)
      obj.updateMin(lbObj + epsilon)
    else {
      obj.updateMin(lbObj)
      card.updateMin(lbCard+1)
    }
  }

  override def updateLB(newLB: Double): Unit = {
    bestObj = bestObj.max(newLB)
    lbObj = bestObj
    bestCard = card.value
    lbCard = card.value
  }
}
