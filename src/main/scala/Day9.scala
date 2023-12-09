import Utils._

import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day9/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Int =
    parse(rows).sumBy { row =>
      step(row, List(row), _ + _)
    }

  def part2(rows: List[String]): Int =
    parse(rows).sumBy { row =>
      step(row, List(row), _ - _)
    }

  @tailrec
  def step(currentRow: List[Int], rowHistory: List[List[Int]], op: (Int, Int) => Int): Int =
    if (currentRow.forall(_ == 0)) {
      rowHistory.tail.foldLeft(0) { case (n, row) => op(row.last, n) }
    } else {
      val newRow =
        currentRow
          .sliding(2)
          .collect { case List(first, second) => second - first }
          .toList

      step(newRow, newRow :: rowHistory, op)
    }

  def parse(rows: List[String]): List[List[Int]] =
    rows.map(_.split(" ").toList.map(_.toInt))
}
