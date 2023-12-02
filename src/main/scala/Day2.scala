import scala.Function.unlift
import scala.io.Source

object Day2 {

  private val maxReds: Int   = 12
  private val maxGreens: Int = 13
  private val maxBlues: Int  = 14

  final case class Game(id: Int, reds: List[Int], greens: List[Int], blues: List[Int]) {
    def feasible: Boolean = reds.forall(_ <= maxReds) && greens.forall(_ <= maxGreens) && blues.forall(_ <= maxBlues)

    def totalReds: Int   = reds.maxOption.getOrElse(0)
    def totalBlues: Int  = blues.maxOption.getOrElse(0)
    def totalGreens: Int = greens.maxOption.getOrElse(0)
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day2/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  // part 2

  def part2(rows: List[String]): Int =
    rows
      .map(getPower)
      .sum

  def getPower(row: String): Int = {
    val game = parseRow(row)
    game.totalReds * game.totalGreens * game.totalBlues
  }

  // part 1

  def part1(rows: List[String]): Int =
    rows
      .collect(unlift(feasibleGameId))
      .sum

  def feasibleGameId(row: String): Option[Int] = {
    val game = parseRow(row)
    Option.when(game.feasible)(game.id)
  }

  def parseRow(input: String): Game = {
    val game  = "Game ([0-9]+): (.*)".r
    val red   = "([0-9]+) red".r
    val green = "([0-9]+) green".r
    val blue  = "([0-9]+) blue".r

    input match {
      case game(id, tail) =>
        val colors =
          tail
            .split("; ")
            .map { subset =>
              subset
                .split(", ")
                .foldLeft((0, 0, 0)) {
                  case ((r, g, b), red(next))   => (r + next.toInt, g, b)
                  case ((r, g, b), green(next)) => (r, g + next.toInt, b)
                  case ((r, g, b), blue(next))  => (r, g, b + next.toInt)
                }
            }

        val (reds, greens, blues) = colors.unzip3
        Game(id = id.toInt, reds = reds.toList, greens = greens.toList, blues = blues.toList)
    }
  }
}
