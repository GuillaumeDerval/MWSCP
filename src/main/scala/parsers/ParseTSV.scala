package parsers

import scala.io.Source

object ParseTSV {
  def get(filename: String) = {
    val matrix: Array[Array[Double]] = Source.fromFile(filename).getLines().toArray.map(_.split("\t").map(x => x.toDouble))

    val nRows = matrix.length
    val nCols = matrix(0).length

    (matrix, nRows, nCols)
  }

  def getSol(filename: String): (Array[Array[Int]], Array[Array[Int]]) = {
    val matrix: Array[Array[Array[Int]]] = Source.fromFile(filename).getLines().toArray.map(_.split("\t").map(parseArray))

    val rows = matrix.map(_(0))
    val cols = matrix.map(_(1))

    (rows, cols)
  }

  def parseArray(line: String): Array[Int] = {
    line.split("\\[")(1).split("\\]")(0).split(", ").map(_.toInt)
  }
}
