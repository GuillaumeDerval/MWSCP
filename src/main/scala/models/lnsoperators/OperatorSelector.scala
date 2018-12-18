package models.lnsoperators

import scala.util.Random

class OperatorSelector(operators: Map[Operator, Double], random: Random) {
  val keys = operators.keys.toArray
  val values = keys.map(operators(_))
  val valuesRunning: Array[Double] = values.scanLeft(0.0)(_ + _)

  val maxSum = valuesRunning.last
  def select(): Operator = {
    val v = random.nextDouble() * maxSum
    for(i <- keys.indices)
      if(v <= valuesRunning(i+1))
        return keys(i)
    keys.last
  }
}
