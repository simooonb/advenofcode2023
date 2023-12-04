import Utils._

import scala.annotation.tailrec
import scala.io.Source

object Day4 {

  final case class Card(winnings: List[Int], owned: List[Int]) {
    val ownedWinningsSize: Int = owned.count(winnings.contains)
    val points: Int            = if (ownedWinningsSize == 0) 0 else math.pow(2, ownedWinningsSize - 1).toInt
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day4/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Int = {
    rows.flatMap(parseRow).sumBy(_.points)
  }

  def part2(rows: List[String]): Int = {
    val originalCardsWithIndex = rows.zipWithIndex
      .map { case (row, i) => i -> parseRow(row).get }

    val originalCards = originalCardsWithIndex.map(_._2)

    @tailrec
    def step(left: List[(Int, Card)], acc: List[Card]): Int =
      left match {
        case Nil => acc.size
        case (i, nextCard) :: tail =>
          val additionalCardsIndices = (1 to nextCard.ownedWinningsSize).toList.map(i + _)
          val additionalCards        = additionalCardsIndices.map(i => i -> originalCards(i))
          step(additionalCards ++ tail, nextCard :: acc)
      }

    step(originalCardsWithIndex, Nil)
  }

  def parseRow(row: String): Option[Card] = {
    val card     = "Card ([0-9]*): (.*)$".r
    val oneSpace = row.trim.replaceAll(" +", " ")

    oneSpace match {
      case card(_, tail) =>
        val Array(winnings, owned) = tail.split(" \\| ")

        Some(
          Card(
            winnings = winnings.split(" ").toList.map(_.trim.toInt),
            owned = owned.split(" ").toList.map(_.trim.toInt)
          )
        )

      case _ =>
        None
    }
  }
}
