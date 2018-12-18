package parsers

import scala.io.Source

class Entry(val year: Int, val sport: String, val discipline: String, val country: String, val gender: Int, val event: String, val medal: Int)

object ParseOlympic {
  val genders = Map("Men" -> 0, "Women" -> 1)
  val medalType = Map("Gold" -> 0, "Silver" -> 1, "Bronze" -> 2)

  def get(theta: Double=0.005) = {
    val data = Source.fromFile("data/olympic/medals.csv").getLines().toArray

    val allEvents = for (l <- data) yield {
      val line = l.split(";")
      new Entry(line(0).toInt,line(1), line(2), line(3), genders(line(4)), line(5), medalType(line(6)))
    }

    val countries = allEvents.map(_.country).distinct
    val discs = allEvents.map(x => (x.sport, x.discipline)).distinct

    val countryMap: Map[String,Int] = countries.zipWithIndex.toMap
    val discMap: Map[(String, String),Int] = discs.zipWithIndex.toMap

    val matrix = Array.fill(discMap.size, countryMap.size)(0.0)

    for(e <- allEvents) {
      matrix(discMap((e.sport, e.discipline)))(countryMap(e.country)) += 1.0
    }

    val matrixLineSum = matrix.map(_.sum)

    for(i <- discs.indices) {
      for (j <- countries.indices) {
        matrix(i)(j) /= matrixLineSum(i)
        matrix(i)(j) -= theta
      }
    }

    (matrix, discs, countries, discMap.size, countryMap.size)
  }
}
