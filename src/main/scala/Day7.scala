import Utils.ListOps

import scala.io.Source
import scala.math.Ordering.Implicits.seqOrdering

object Day7 {

  sealed trait Card {
    def str: String

    def weight: Int
  }

  case object A     extends Card { val str: String = "A"; val weight: Int = 14 }
  case object K     extends Card { val str: String = "K"; val weight: Int = 13 }
  case object Q     extends Card { val str: String = "Q"; val weight: Int = 12 }
  case object J     extends Card { val str: String = "J"; val weight: Int = 11 }
  case object Joker extends Card { val str: String = "J"; val weight: Int = 1  }
  case object T     extends Card { val str: String = "T"; val weight: Int = 10 }
  case object Nine  extends Card { val str: String = "9"; val weight: Int = 9  }
  case object Eight extends Card { val str: String = "8"; val weight: Int = 8  }
  case object Seven extends Card { val str: String = "7"; val weight: Int = 7  }
  case object Six   extends Card { val str: String = "6"; val weight: Int = 6  }
  case object Five  extends Card { val str: String = "5"; val weight: Int = 5  }
  case object Four  extends Card { val str: String = "4"; val weight: Int = 4  }
  case object Three extends Card { val str: String = "3"; val weight: Int = 3  }
  case object Two   extends Card { val str: String = "2"; val weight: Int = 2  }

  object Card {
    val allPart1: List[Card] = List(A, K, Q, J, T, Nine, Eight, Seven, Six, Five, Four, Three, Two)
    val allPart2: List[Card] = List(A, K, Q, Joker, T, Nine, Eight, Seven, Six, Five, Four, Three, Two)

    def fromPart1(c: String): Option[Card] =
      allPart1.find(_.str == c)

    def fromPart2(c: String): Option[Card] =
      allPart2.find(_.str == c)
  }

  sealed trait HandType {
    def weight: Int
  }

  case object FiveOfAKind  extends HandType { val weight: Int = 7 }
  case object FourOfAKind  extends HandType { val weight: Int = 6 }
  case object FullHouse    extends HandType { val weight: Int = 5 }
  case object ThreeOfAKind extends HandType { val weight: Int = 4 }
  case object TwoPair      extends HandType { val weight: Int = 3 }
  case object OnePair      extends HandType { val weight: Int = 2 }
  case object HighCard     extends HandType { val weight: Int = 1 }

  final case class Hand(cards: List[Card]) {
    val occurrencesCount: Map[Card, Int] = cards.groupBy(identity).view.mapValues(_.size).toMap

    private val jokerCount: Int = occurrencesCount.getOrElse(Joker, 0)

    def has5: Option[FiveOfAKind.type] =
      existsCount(5, withJoker = true).map(_ => FiveOfAKind)

    def has4: Option[FourOfAKind.type] =
      existsCount(4, withJoker = true).map(_ => FourOfAKind)

    def hasFullHouse: Option[FullHouse.type] =
      for {
        three <- existsCount(3, withJoker = true)
        _     <- existsCount(2, withJoker = false, excluded = List(three, Joker))
      } yield FullHouse

    def has3: Option[ThreeOfAKind.type] =
      existsCount(3, withJoker = true).map(_ => ThreeOfAKind)

    def has2Pair: Option[TwoPair.type] =
      for {
        first <- existsCount(2, withJoker = true)
        _     <- existsCount(2, withJoker = false, excluded = List(first, Joker))
      } yield TwoPair

    def hasPair: Option[OnePair.type] =
      existsCount(2, withJoker = true).map(_ => OnePair)

    val handType: HandType =
      (has5 orElse has4 orElse hasFullHouse orElse has3 orElse has2Pair orElse hasPair)
        .getOrElse(HighCard)

    private def existsCount(count: Int, withJoker: Boolean, excluded: List[Card] = Nil): Option[Card] = {
      val jc = if (withJoker) jokerCount else 0
      occurrencesCount.collectFirst {
        case (Joker, n) if n == count && !excluded.contains(Joker)                       => Joker
        case (card, n) if card != Joker && (n + jc) == count && !excluded.contains(card) => card
      }
    }
  }

  final case class HandWithBid(hand: Hand, bid: Int)

  val byCardHandOrdering: Ordering[Hand]    = Ordering.by(_.cards.map(-_.weight))
  val byTypeHandOrdering: Ordering[Hand]    = Ordering.by(-_.handType.weight)
  implicit val handOrdering: Ordering[Hand] = byTypeHandOrdering.orElse(byCardHandOrdering)

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day7/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Int = {
    val hands: List[HandWithBid] = parsePart1(rows)

    hands.sortBy(_.hand)(handOrdering).zipWithIndex.sumBy { case (hwb, i) =>
      val rank = hands.size - i
      hwb.bid * rank
    }
  }

  def part2(rows: List[String]): Int = {
    val hands: List[HandWithBid] = parsePart2(rows)

    hands.sortBy(_.hand)(handOrdering).zipWithIndex.sumBy { case (hwb, i) =>
      val rank = hands.size - i
      hwb.bid * rank
    }
  }

  def parsePart1(rows: List[String]): List[HandWithBid] =
    rows.map { row =>
      val Array(hand, bid) = row.split(" ")
      val cards            = hand.flatMap(c => Card.fromPart1(c.toString))

      HandWithBid(Hand(cards.toList), bid.toInt)
    }

  def parsePart2(rows: List[String]): List[HandWithBid] =
    rows.map { row =>
      val Array(hand, bid) = row.split(" ")
      val cards            = hand.flatMap(c => Card.fromPart2(c.toString))

      HandWithBid(Hand(cards.toList), bid.toInt)
    }
}
