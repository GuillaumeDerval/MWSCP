package utils

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleDouble
import oscar.cp.CPStore

class DoubleVar(baseMin: Double, baseMax: Double, val epsilon: Double = 1e-8)(implicit val cp: CPStore) {
  private[this] val vmin = new ReversibleDouble(cp, baseMin)
  private[this] val vmax = new ReversibleDouble(cp, baseMax)

  if(vmin > vmax)
    throw Inconsistency

  @inline
  def almostEqual(a: Double, b: Double): Boolean = Math.abs(a-b) < epsilon

  @inline
  def updateMin(newMin: Double): Unit = {
    if(vmax.value < newMin && !almostEqual(vmax.value, newMin))
      throw Inconsistency
    if(vmin.value < newMin)
      vmin.value = newMin
  }

  @inline
  def updateMax(newMax: Double): Unit = {
    if(vmin.value > newMax && !almostEqual(vmin.value, newMax))
      throw Inconsistency
    if(vmax.value > newMax)
      vmax.value = newMax
  }

  def assign(value: Double): Unit = {
    if(vmin.value > value || vmax.value < value)
      throw Inconsistency
    vmin.value = value
    vmax.value = value
  }

  def min = vmin.value
  def max = vmax.value
  def value = {
    if(!almostEqual(vmin.value, vmax.value))
      throw new Exception("unbound %s %s".format(min, max))
    vmin.value
  }
}
