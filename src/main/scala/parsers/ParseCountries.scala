package parsers

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

class Country(val name: String, val HI: Boolean, val position: String, val population: Array[Double]) {
  override def toString: String = {
    name
    //s"$name, HI:$HI, pos:$position population:"+population.mkString("\t")
  }
}

object ParseCountries {
  def get(theta: Double=0.005) = {
    val dataCountries = Source.fromFile("data/immigration/characteristics.csv").getLines().toArray

    val countries = for (l <- dataCountries) yield {
      val line = l.split(";")
      new Country(line(0),line(2).toInt == 1, line(1),line.drop(3).map(i => i.toDouble * 1e6))
      //Azerbaijan;S;0;3.898;5.178;6.164;7.270;8.114;9.187;9.754

    }

    val nCountries = countries.size

    val countryMap: Map[String,Int] = countries.map(_.name).zipWithIndex.toMap


    //for (c <- countries) {
    //  if (c.name.startsWith("Bel")) {
    //    println(c+" "+c.population(0))
    //  }
    //}

    // ---------------------------


    val dataImmigration = Source.fromFile("data/immigration/immigration.csv").getLines().toArray

    val immigrationMap: Map[(String,String),Array[Double]]= (for (l <- dataImmigration) yield {
      val line = l.split(";")
      val c1 = line(0).trim()
      val c2 = line(1).trim()
      val data = line.drop(2).map(_.toDouble)
      (c1,c2) -> data
    }).toMap

    // -----------------------------

    //val yearIdx1 = 0
    val yearIdx2 = 6

    val rowCountries = countries.filter(c => c.HI == false && (c.population(yearIdx2) >= 10000000))
    val colCountries = countries.filter(c => c.HI == true)
    val rowMap = rowCountries.map(_.name).zipWithIndex.toMap
    val colMap = colCountries.map(_.name).zipWithIndex.toMap
    val nRows = rowCountries.size
    val nCols = colCountries.size


    //println("nRows:"+nRows+" nCols:"+nCols)

    val matrix = Array.fill(nRows,nCols)(0.0)



    for (i <- 0 until nRows; j <- 0 until nCols) {
      val c1: String = rowCountries(i).name
      val c2: String = colCountries(j).name
      //val before = immigrationMap((c1,c2))(yearIdx1)
      val after = immigrationMap((c1,c2))(yearIdx2)
      matrix(i)(j) = after/countries(countryMap(c1)).population(yearIdx2)-theta
    }

    (matrix, rowCountries, colCountries, nRows, nCols)
  }


}

object Gen extends App {
  for(theta <- Array(0.001, 0.003, 0.005)) {
    val (matrix, rowCountries, colCountries, _, _) = ParseCountries.get(theta)
    val file = new File("data/immigration_"+theta+".tsv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(matrix.map(_.mkString("\t")).mkString("\n"))
    bw.close()
  }

}