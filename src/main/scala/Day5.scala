import Utils._

import scala.Function.unlift
import scala.annotation.tailrec
import scala.io.Source

object Day5 {

  final case class NumberRange(from: Long, length: Long) {
    val to: Long = from + length - 1

    def allNumbers: List[Long] = (from to to).toList

    def has(i: Long): Boolean =
      from <= i && i <= to
  }

  final case class TranslationMap(source: NumberRange, to: NumberRange) {
    def translate(i: Long): Option[Long] = {
      Option.when(source.has(i)) {
        i - source.from + to.from
      }
    }
  }

  final case class GetTranslation(i: Long) {
    def unapply(map: TranslationMap): Option[Long] =
      map.translate(i)
  }

  final case class Translation(from: String, to: String, maps: List[TranslationMap]) {
    def translate(origin: Long): Long = {
      val get = GetTranslation(origin)
      maps.collectFirst(unlift(get.unapply)).getOrElse(origin)
    }

  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day5/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Long = {

    val (seeds, maps) = parse(rows)

    @tailrec
    def step(current: Long, sourceName: String): Long =
      if (sourceName == "location")
        current
      else {
        val map        = maps.find(_.from == sourceName).get
        val translated = map.translate(current)

        step(translated, map.to)
      }

    val locations = seeds.map(seed => step(seed, "seed"))

    locations.min
  }

  def part2(rows: List[String]): Long = {

    val (seeds, maps) = parse(rows)

    val seedRanges = seeds.grouped(2).toList.flatMap {
      case List(start, length) => Some(NumberRange(start, length))
      case _                   => None
    }

    @tailrec
    def step(current: Long, sourceName: String): Long =
      if (sourceName == "location")
        current
      else {
        val map        = maps.find(_.from == sourceName).get
        val translated = map.translate(current)

        step(translated, map.to)
      }

    var minLocation = Long.MaxValue

    seedRanges.foreach { seedRange =>
      (0L to seedRange.length).foreach { i =>
        val seed     = seedRange.from + i
        val location = step(seed, "seed")
        if (location < minLocation)
          minLocation = location
      }
    }

    minLocation
  }

  def parse(rows: List[String]): (List[Long], List[Translation]) = {

    val translateRegex = "([a-z]+)-to-([a-z]+) map:$".r

    val seedsRow :: tail = rows

    val seeds = "seeds: ([0-9 ]+)$".r.findFirstMatchIn(seedsRow).get.group(1).split(" ").map(_.toLong).toList

    val maps = tail.foldLeft(List.empty[Translation]) {
      case (acc, row) if row.isEmpty       => acc.reverse
      case (acc, translateRegex(from, to)) => Translation(from, to, Nil) :: acc
      case (acc, row) =>
        val Array(dest, src, length) = row.split(" ")
        val source                   = NumberRange(src.toLong, length.toLong)
        val destination              = NumberRange(dest.toLong, length.toLong)
        val updatedTranslation       = acc.head.copy(maps = acc.head.maps :+ TranslationMap(source, destination))
        acc.updated(0, updatedTranslation)
    }

    (seeds, maps)
  }
}
