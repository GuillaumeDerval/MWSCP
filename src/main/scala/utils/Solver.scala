package utils
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}
case class Solution(obj: Double, rows: Array[Array[Int]], cols: Array[Array[Int]], time: Long, comment: String = null)

trait Solver {
  def solve(matrix: Array[Array[Double]], k: Int, queue: LinkedBlockingQueue[Solution]): Unit
}