package utils

import oscar.algo.reversible.{ReversibleContext, ReversibleInt, ReversibleSparseSubset}

class ReversibleSparseSubsetSupport(store: ReversibleContext, val min: Int, val max: Int, val initialSupports: Int) {

  val size1 = new ReversibleInt(store, 0) // subset initially empty
  val size2 = new ReversibleInt(store, max - min + 1) // superset contains everything

  val values = Array.tabulate(size2.value)(i => i)
  val indexes = Array.tabulate(size2.value)(i => i)
  val supports = Array.fill(size2.value)(new ReversibleInt(store, initialSupports))

  def possibleSize = size2.value
  def requiredSize = size1.value

  def decreaseSupport(value: Int): Unit = {
    checkVal(value)
    if(isPossible(value)) {
      supports(value-min) -= 1
      if(supports(value-min).value == 0)
        excludes(value)
    }
  }

  def setSupport(value: Int, nSupports: Int): Unit = {
    checkVal(value)
    if(isPossible(value) && supports(value-min).value >= nSupports) {
      supports(value-min).value = nSupports
      if(supports(value-min).value == 0)
        excludes(value)
    }
    else if(isPossible(value) || nSupports != 0)
      throw new RuntimeException("New support is superior to old one!")
  }

  def requires(value: Int) {
    checkVal(value)
    if (isRequired(value))
      return
    if (!isPossible(value))
      throw new RuntimeException(value + " cannot be required since it is even not possible")
    exchangePositions(value, values(size1.value) + min)
    size1.incr()
    assert(size1.value <= values.length)
  }

  @inline
  private def excludes(value: Int) {
    checkVal(value)
    if (!isPossible(value)) return // it is already not possible
    if (isRequired(value)) throw new RuntimeException(value + " is required so it cannot be excluded")
    supports(value-min).value = 0
    exchangePositions(value, values(size2.value - 1) + min)
    size2.decr()
    assert(size1.value <= values.length)
  }

  private def exchangePositions(value1: Int, value2: Int) {
    checkVal(value1)
    checkVal(value2)
    val v1 = value1 - min
    val v2 = value2 - min
    val i1 = indexes(v1)
    val i2 = indexes(v2)
    values(i1) = v2
    values(i2) = v1
    indexes(v1) = i2
    indexes(v2) = i1
  }

  def nSupports(value: Int): Int = supports(value-min).value

  def isRequired(value: Int): Boolean = {
    if (value < min || value > max) false;
    else indexes(value - min) < size1.value;
  }

  def isPossible(value: Int): Boolean = {
    if (value < min || value > max) false;
    else indexes(value - min) < size2.value
  }

  def fillArrayPossible(x: Array[Int]): Int = {
    var i = 0
    while(i < size2.value) {
      x(i) = values(i)+min
      i += 1
    }
    i
  }

  def fillArrayRequired(x: Array[Int]): Int = {
    var i = 0
    while(i < size1.value) {
      x(i) = values(i)+min
      i += 1
    }
    i
  }

  def fillArrayExcluded(x: Array[Int]): Int = {
    var i = size2.value
    while(i < values.length) {
      x(i) = values(i)+min
      i += 1
    }
    i-size2.value
  }

  def fillArrayPossible(x: Array[Int], excludingOneSupport: ReversibleSparseSubset, excludingPossible: Boolean): Int = {
    val check: Int => Boolean = if(excludingPossible) excludingOneSupport.isRequired else excludingOneSupport.isPossible

    var i = 0
    var j = 0
    while(i < size2.value) {
      if(supports(values(i)).value != 1 || !check(values(i)+min)) {
        x(j) = values(i)
        j += 1
      }
      i += 1
    }
    j
  }

  def fillArrayRequired(x: Array[Int], excludingOneSupport: ReversibleSparseSubset, excludingPossible: Boolean): Int = {
    val check: Int => Boolean = if(excludingPossible) excludingOneSupport.isRequired else excludingOneSupport.isPossible

    var i = 0
    var j = 0
    while(i < size1.value) {
      if(supports(values(i)).value != 1 || !check(values(i)+min)) {
        x(j) = values(i)
        j += 1
      }
      i += 1
    }
    j
  }

  @inline
  def checkVal(value: Int) = {
    assert(value >= min)
    assert(value <= max)
  }
}