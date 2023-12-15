import scala.io.Source
import scala.math.abs

object Day11 {
  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day11/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Long =
    compute(rows, 2)

  def part2(rows: List[String]): Long =
    compute(rows, 1000000)

  def compute(rows: List[String], expansion: Long): Long = {
    val galaxies = rows.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect {
        case (c, x) if c == '#' => (x, y)
      }.toList
    }

    val emptyRowsIndices = rows.zipWithIndex.collect {
      case (row, y) if row.forall(_ == '.') => y
    }

    val emptyColumnsIndices = rows.head.indices.flatMap { x =>
      Option.when(rows.forall(_(x) == '.'))(x)
    }

    val newGalaxiesPos =
      galaxies.map { case (x, y) =>
        val offsetX = emptyColumnsIndices.count(_ < x) * (expansion - 1)
        val offsetY = emptyRowsIndices.count(_ < y) * (expansion - 1)

        (offsetX + x, offsetY + y)
      }

    // Manhattan distance
    def distance(fromX: Long, fromY: Long, toX: Long, toY: Long): Long =
      abs(toX - fromX) + abs(toY - fromY)

    newGalaxiesPos
      .combinations(2)
      .map {
        case List((x1, y1), (x2, y2)) => distance(x1, y1, x2, y2)
        case _                        => 0
      }
      .sum
  }
}
