package apps

import utils.Solution
import javax.swing.{JFrame, JPanel, SwingUtilities}
import java.awt.{Color, Graphics}

object SolveGraphic extends SolveBase() {
  def newSol(sol: Solution): Unit = {
    println("New best bound: " + sol.obj)
    println("Time: " + sol.time)
    if(best != -1.0)
      println("Ratio and abs diff: "+ (sol.obj/best) + " " + (sol.obj - best))
    println("Rows [" + sol.rows.map(_.mkString(",")).mkString("],\t[") + "]")
    println("Cols [" + sol.cols.map(_.mkString(",")).mkString("],\t[") + "]")
    if (sol.comment != null)
      println("Comment " + sol.comment)
    println("---")

    show.selectedRows = sol.rows
    show.selectedCols = sol.cols

    show.repaint()
  }



  val frame: JFrame = new JFrame
  val show = new Show(matrix)
  frame.setSize(nRows+100, nCols+100)
  frame.add(show)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setVisible(true)

  run()
}


class Show(matrix: Array[Array[Double]]) extends JPanel {

  var selectedRows : Array[Array[Int]] = null
  var selectedCols : Array[Array[Int]] = null

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    //g.fillRect(100, 100, 400, 400)
    g.setColor(Color.BLACK)
    for(i <- 0 until matrix.length)
      for(j <- 0 until matrix(0).length)
        if(matrix(i)(j) > 0)
          g.fillRect(i,j,1,1)
    g.setColor(Color.GRAY)
    for(i <- 0 until matrix.length)
      for(j <- 0 until matrix(0).length)
        if(matrix(i)(j) < 0)
          g.fillRect(i,j,1,1)
    if(selectedRows != null) {
      for(k <- 0 until selectedRows.length) {
        for (i <- selectedRows(k)) {
          for (j <- selectedCols(k)) {
            if (matrix(i)(j) > 0) {
              g.setColor(Color.GREEN)
              g.fillRect(i, j, 1, 1)
            }
            else if (matrix(i)(j) < 0) {
              g.setColor(Color.RED)
              g.fillRect(i, j, 1, 1)
            }
          }
        }
      }
    }
  }
}
