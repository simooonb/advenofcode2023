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

  def part1(rows: List[String]): Int = {
    val allRows = parse(rows)

    @tailrec
    def step(currentRow: List[Int], rowHistory: List[List[Int]]): Int =
      if (currentRow.forall(_ == 0)) {
        rowHistory.tail.foldLeft(0) { case (n, row) => row.last + n }
      } else {
        val newRow =
          currentRow
            .sliding(2)
            .collect { case List(first, second) => second - first }
            .toList

        step(newRow, newRow :: rowHistory)
      }

    allRows.sumBy { row =>
      step(row, List(row))
    }
  }

  def part2(rows: List[String]): Int = {
    val allRows = parse(rows)

    @tailrec
    def step(currentRow: List[Int], rowHistory: List[List[Int]]): Int =
      if (currentRow.forall(_ == 0)) {
        rowHistory.tail.foldLeft(0) { case (n, row) => row.head - n }
      } else {
        val newRow =
          currentRow
            .sliding(2)
            .collect { case List(first, second) => second - first }
            .toList

        step(newRow, newRow :: rowHistory)
      }

    allRows.sumBy { row =>
      step(row, List(row))
    }
  }

  def parse(rows: List[String]): List[List[Int]] =
    rows.map(_.split(" ").toList.map(_.toInt))
}
