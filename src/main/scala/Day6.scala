import scala.Function.unlift
import scala.annotation.tailrec
import scala.io.Source

object Day6 {

  final case class Race(totalTime: Long, recordDistance: Long) {
    val actions: List[Hold] = (0L to totalTime).toList.map { heldTime =>
      Hold(heldTime, totalTime - heldTime)
    }
  }

  final case class Hold(heldTime: Long, timeLeft: Long) {
    val speed: Long           = heldTime
    val distanceReached: Long = speed * timeLeft
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day6/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Int = {
    val races = parsePart1(rows)

    races.foreach(println)

    races.map { race =>
      race.actions.count(_.distanceReached > race.recordDistance)
    }.product
  }

  def part2(rows: List[String]): Int = {
    val race = parsePart2(rows)

    println(race)

    (0L to race.totalTime).iterator.count(x => race.totalTime * x - x * x > race.recordDistance)

//    race.actions.count(_.distanceReached > race.recordDistance)
  }

  def parsePart1(rows: List[String]): List[Race] = {
    val timeRow :: distanceRow :: Nil = rows

    val times     = timeRow.drop(5).trim.split(" ").collect { case s if s.nonEmpty => s.toLong }.toList
    val distances = distanceRow.drop(9).trim.split(" ").collect { case s if s.nonEmpty => s.toLong }.toList

    (times zip distances).map { case (time, distance) => Race(time, distance) }
  }

  def parsePart2(rows: List[String]): Race = {
    val timeRow :: distanceRow :: Nil = rows

    val time     = timeRow.drop(5).trim.split(" ").collect { case s if s.nonEmpty => s }.toList.mkString("").toLong
    val distance = distanceRow.drop(9).trim.split(" ").collect { case s if s.nonEmpty => s }.toList.mkString("").toLong

    println(time)
    println(distance)

    Race(time, distance)
  }
}
