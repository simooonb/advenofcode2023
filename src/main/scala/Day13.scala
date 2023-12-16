import Utils._

import scala.Function.unlift
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day13 {
  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day13/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Int = {
    val patterns = parse(rows)

    patterns.sumBy { pattern =>
      findVerticalReflection(pattern).getOrElse(0) + 100 * findHorizontalReflection(pattern).getOrElse(0)
    }
  }

  def part2(rows: List[String]): Int = {
    val patterns = parse(rows)

    patterns.sumBy { pattern =>
      val origV      = findVerticalReflection(pattern)
      val origH      = findHorizontalReflection(pattern)
      val candidates = smudgeCandidates(pattern)

      candidates
        .collectFirst(unlift(getReflection(origV, origH)))
        .map { case (v, h) => v + 100 * h }
        .getOrElse(0)
    }
  }

  def getReflection(originalVertical: Option[Int], originalHorizontal: Option[Int])(
      pattern: List[String]
  ): Option[(Int, Int)] = {
    val vOpt = findVerticalReflection(pattern, exclude = originalVertical.map(_ - 1))
    val hOpt = findHorizontalReflection(pattern, exclude = originalHorizontal.map(_ - 1))

    Option.when(vOpt.nonEmpty || hOpt.nonEmpty) {
      vOpt.getOrElse(0) -> hOpt.getOrElse(0)
    }
  }

  def findHorizontalReflection(pattern: List[String], exclude: Option[Int] = None): Option[Int] =
    pattern.indices.init
      .filterNot(exclude.contains)
      .find { i =>
        val maxOffset = (pattern.size - (i + 1)) min (i + 1)

        (0 until maxOffset).forall { offset =>
          pattern(i - offset) == pattern(i + 1 + offset)
        }
      }
      .map(_ + 1)

  def findVerticalReflection(pattern: List[String], exclude: Option[Int] = None): Option[Int] = {
    val rowSize = pattern.head.length

    (0 until rowSize - 1)
      .filterNot(exclude.contains)
      .find { j =>
        val maxOffset = (rowSize - (j + 1)) min (j + 1)

        (0 until maxOffset).forall { offset =>
          pattern.map(_(j - offset)) == pattern.map(_(j + 1 + offset))
        }
      }
      .map(_ + 1)
  }

  def smudgeCandidates(pattern: List[String]): List[List[String]] = {
    val acc: ListBuffer[List[String]] = ListBuffer.from(Nil)

    pattern.zipWithIndex.foreach { case (row, rowIndex) =>
      row.zipWithIndex.foreach {
        case ('.', charIndex) => acc.addOne(pattern.updated(rowIndex, row.updated(charIndex, '#')))
        case ('#', charIndex) => acc.addOne(pattern.updated(rowIndex, row.updated(charIndex, '.')))
        case _                => ()
      }
    }

    acc.toList
  }

  def parse(rows: List[String]): List[List[String]] = {
    var i = 0

    val rowsGroups =
      rows.map {
        case row if row.isEmpty =>
          i += 1
          row -> i
        case row =>
          row -> i
      }

    rowsGroups
      .filter { case (row, _) => row.nonEmpty }
      .groupBy { case (_, i) => i }
      .toList
      .map { case (_, pattern) => pattern.map { case (row, _) => row } }
  }

}
