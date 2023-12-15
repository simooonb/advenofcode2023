import scala.io.Source
import Utils._

import scala.collection.mutable

object Day15 {
  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day15/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Long =
    rows.head.split(",").toList.sumBy(hash)

  def part2(rows: List[String]): Long = {
    val boxes: mutable.Map[Long, mutable.Map[String, (Long, Long)]] = mutable.Map.from(
      List.tabulate(256)(_.toLong -> mutable.Map.empty)
    )

    rows.head.split(",").toList.foreach { row =>
      if (row.contains('=')) {
        val Array(label, focalLengthStr) = row.split("=")
        val boxIndex                     = hash(label)
        val box                          = boxes(boxIndex)
        val focalLength                  = focalLengthStr.toInt

        box.get(label) match {
          case Some((position, _)) =>
            box(label) = (position, focalLength)
          case None =>
            val maxPosition = box.values.map { case (position, _) => position }.maxOption.getOrElse(0L)
            box(label) = (maxPosition + 1L, focalLength)
        }
      } else if (row.contains('-')) {
        val Array(label) = row.split("-")
        val boxIndex     = hash(label)
        val box          = boxes(boxIndex)

        box.get(label) match {
          case Some((oldPosition, _)) =>
            box -= label
            box.foreach {
              case (label, (position, focalLength)) if position > oldPosition =>
                box(label) = (position - 1L, focalLength)
              case _ =>
                ()
            }

          case None => ()
        }
      }
    }

    boxes.flatMap { case (boxNumber, slots) =>
      slots.map { case (_, (position, focalLength)) =>
        (boxNumber + 1) * position * focalLength
      }
    }.sum
  }

  def hash(str: String): Int = {
    var currentValue = 0
    str.foreach { char =>
      currentValue += char.toInt
      currentValue = currentValue * 17
      currentValue = currentValue % 256
    }
    currentValue
  }

  def hashForChar(char: Char): Int =
    (char.toInt * 17) % 256
}
