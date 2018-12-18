package contraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPSetVar, CPVar}

class LexLeqSet(x: CPSetVar, y: CPSetVar) extends Constraint(x.store) {
  val minPossible = ReversibleInt(if(x.possibleSize == 0) Integer.MAX_VALUE else x.possibleSet().min)(s)
  val maxPossible = ReversibleInt(Integer.MAX_VALUE)(s)


  override def associatedVars(): Iterable[CPVar] = Array(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    x.callValExcludedWhenExcludedValue(this)
    y.callValRequiredWhenRequiredValue(this)

    if(y.requiredSize != 0)
      maxPossible.value = y.requiredSet().min

    propagate()
  }

  override def valRequired(v: CPSetVar, value: Int): Unit = {
    super.valRequired(v, value)
    assert(v.equals(y))
    if(maxPossible.value > value)
      maxPossible.value = value

    propagate()
  }

  override def valExcluded(v: CPSetVar, value: Int): Unit = {
    super.valRequired(v, value)
    if(minPossible.value == value)
      minPossible.value = minPossible.value + 1
    propagate()
  }

  override def propagate(): Unit = {
    super.propagate()

    if(!x.isPossible(minPossible.value)) {
      if(x.possibleSize == 0)
        minPossible.value = Integer.MAX_VALUE
      else
        minPossible.value = x.possibleSet().min
    }

    if(minPossible.value > maxPossible.value)
      throw Inconsistency
  }
}
