import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day1/input.txt")
    val input  = source.getLines()
    val result = input.map(part2).toList

    println(result)
    println(result.sum)

    source.close()
  }

  def part1(input: String): Int =
    (for {
      first  <- input.find(_.isDigit).map(_ - 48)
      second <- input.findLast(_.isDigit).map(_ - 48)
    } yield s"$first$second".toInt).getOrElse(0)

  def part2(input: String): Int = {
    val conversionTable: Map[String, Int] = Map(
      "1"     -> 1,
      "2"     -> 2,
      "3"     -> 3,
      "4"     -> 4,
      "5"     -> 5,
      "6"     -> 6,
      "7"     -> 7,
      "8"     -> 8,
      "9"     -> 9,
      "one"   -> 1,
      "two"   -> 2,
      "three" -> 3,
      "four"  -> 4,
      "five"  -> 5,
      "six"   -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine"  -> 9
    )

    val firstSubStringsPos =
      for {
        subString <- conversionTable.keys.toList
        index = input.indexOfSlice(subString)
        if index != -1
      } yield index -> subString

    val lastSubStringsPos =
      for {
        subString <- conversionTable.keys.toList
        index = input.lastIndexOfSlice(subString)
        if index != -1
      } yield index -> subString

    val pos                  = firstSubStringsPos ++ lastSubStringsPos
    val (_, firstSubString)  = pos.minBy { case (i, _) => i }
    val (_, secondSubString) = pos.maxBy { case (i, _) => i }

    val first  = conversionTable(firstSubString)
    val second = conversionTable(secondSubString)

    s"$first$second".toInt
  }
}
