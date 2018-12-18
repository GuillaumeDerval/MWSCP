package apps

import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}

import models._
import parsers.ParseTSV
import utils.Solution

abstract class SolveBase(needSol: Boolean = false) extends App {
  val availableSolvers = Map(
    "ex" -> GenericEx,
    "lnscut" -> new GenericLNSCut(true)
  )

  val solverCls = availableSolvers(args(0))
  val nSubmatrices = args(1).toInt
  var (matrix, nRows, nCols) = ParseTSV.get(args(2))
  val (solRows, solCols): (Array[Array[Int]], Array[Array[Int]]) = if(needSol || args.length > 3) ParseTSV.getSol(args(3)) else (null, null)

  val best: Double = if(needSol || args.length > 3) computeBest() else -1.0

  /*
  val rowOrder = scala.util.Random.shuffle((0 until nRows).toList)
  val colOrder = scala.util.Random.shuffle((0 until nCols).toList)

  matrix = rowOrder.map(i => colOrder.map(j => matrix(i)(j)).toArray).toArray*/

  val queue = new LinkedBlockingQueue[Solution]()

  def run() = {
    val thread = new Thread(() => {
      solverCls.solve(matrix, nSubmatrices, queue)
    })
    thread.start()

    var done = false
    while (!done) {
      val sol = queue.take()
      if(sol.rows == null)
        done = true
      else
        newSol(sol)
    }
  }

  private def computeBest(): Double = {
    val checkerboard = Array.fill(nRows, nCols)(false)
    for(k <- 0 until nSubmatrices) {
      for(i <- solRows(k))
        for(j <- solCols(k))
          checkerboard(i)(j) = true
    }
    var c = 0.0
    for(i <- 0 until nRows)
      for(j <- 0 until nCols)
        if(checkerboard(i)(j))
          c += matrix(i)(j)
    c
  }

  def newSol(sol: Solution): Unit
}
