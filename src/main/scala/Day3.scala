import scala.io.Source
import Utils._

object Day3 {

  final case class PartNumber(x: Int, yStart: Int, yEnd: Int, numberStr: String) {
    val number: Int = numberStr.toInt

    def contains(otherX: Int, otherY: Int): Boolean =
      x == otherX && yStart <= otherY && otherY <= yEnd
  }

  final case class Symbol(x: Int, y: Int) {
    def neighbours(maxX: Int, maxY: Int): List[(Int, Int)] =
      for {
        newX <- (x - 1 to x + 1).toList
        newY <- (y - 1 to y + 1).toList
        if newX >= 0 && newY >= 0 && newX <= maxX && newY <= maxY
      } yield (newX, newY)
  }

  final case class Engine(map: Array[Array[Char]]) {
    val rowsSize: Int = map.length
    val colSize: Int  = map.head.length

    def allNumbers: List[PartNumber] =
      (0 until rowsSize).toList.flatMap(numbersInRow)

    def allSymbols: List[Symbol] =
      (0 until rowsSize).toList.flatMap(symbolsInRow)

    def allStars: List[Symbol] =
      (0 until rowsSize).toList.flatMap(starsInRow)

    def numbersInRow(x: Int): List[PartNumber] = {
      val (numbers, _) =
        map(x).zipWithIndex.foldLeft(List.empty[PartNumber], false) {
          case ((numbers, false), (next, col)) if next.isDigit =>
            (numbers :+ PartNumber(x, col, col, s"$next")) -> true

          case ((numbers, true), (next, col)) if next.isDigit =>
            val last        = numbers.last
            val lastUpdated = last.copy(yEnd = col, numberStr = last.numberStr + s"$next")
            (numbers.init :+ lastUpdated) -> true

          case ((numbers, _), _) => numbers -> false
        }

      numbers
    }

    def symbolsInRow(x: Int): List[Symbol] =
      map(x).zipWithIndex.collect {
        case (c, y) if !c.isDigit && c != '.' => Symbol(x, y)
      }.toList

    def starsInRow(x: Int): List[Symbol] =
      map(x).zipWithIndex.collect {
        case (c, y) if c == '*' => Symbol(x, y)
      }.toList
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day3/input.txt")
    val input  = source.getLines()
    val result = part2(input.toArray)

    println(result)

    source.close()
  }

  def part1(rows: Array[String]): Int = {
    val engine         = Engine(rows.map(_.toCharArray))
    val allPartNumbers = engine.allNumbers
    val (maxX, maxY)   = (engine.rowsSize, engine.colSize)

    engine.allSymbols
      .flatMap { symbol =>
        val neighbours = symbol.neighbours(maxX, maxY)
        allPartNumbers.filter { number =>
          neighbours.exists((number.contains _).tupled)
        }
      }
      .sumBy(_.number)
  }

  def part2(rows: Array[String]): Int = {
    val engine         = Engine(rows.map(_.toCharArray))
    val allPartNumbers = engine.allNumbers
    val (maxX, maxY)   = (engine.rowsSize, engine.colSize)

    engine.allStars
      .flatMap { symbol =>
        val neighbours = symbol.neighbours(maxX, maxY)
        val numbersNeighbours = allPartNumbers.filter { number =>
          neighbours.exists((number.contains _).tupled)
        }

        Option.when(numbersNeighbours.size == 2)(numbersNeighbours)
      }
      .sumBy(n => n.head.number * n.last.number)
  }
}
