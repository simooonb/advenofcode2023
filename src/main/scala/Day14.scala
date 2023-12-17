import Utils._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day14 {

  sealed trait Space { def char: Char }
  case object RoundRock  extends Space { val char: Char = 'O' }
  case object CubeRock   extends Space { val char: Char = '#' }
  case object EmptySpace extends Space { val char: Char = '.' }

  final case class Position(x: Int, y: Int) {
    def moveSouth: Position = Position(x, y + 1)
    def moveNorth: Position = Position(x, y - 1)
    def moveEast: Position  = Position(x + 1, y)
    def moveWest: Position  = Position(x - 1, y)
  }

  final case class Platform(gridArray: Array[Array[Space]]) {
    val maxX: Int = gridArray.head.length
    val maxY: Int = gridArray.length

    def str: String =
      gridArray.map(_.map(_.char).mkString("")).mkString("\n")

    def roundRocksSortedByNorth(): List[Position] =
      gridArray.zipWithIndex
        .flatMap { case (row, y) =>
          row.zipWithIndex.collect { case (RoundRock, x) => Position(x, y) }
        }
        .sortBy(position => (position.y, position.x))
        .toList

    def roundRocksSortedBySouth(): List[Position] =
      gridArray.zipWithIndex
        .flatMap { case (row, y) =>
          row.zipWithIndex.collect { case (RoundRock, x) => Position(x, y) }
        }
        .sortBy(position => (-position.y, position.x))
        .toList

    def roundRocksSortedByEast(): List[Position] =
      gridArray.zipWithIndex
        .flatMap { case (row, y) =>
          row.zipWithIndex.collect { case (RoundRock, x) => Position(x, y) }
        }
        .sortBy(position => (-position.x, position.y))
        .toList

    def roundRocksSortedByWest(): List[Position] =
      gridArray.zipWithIndex
        .flatMap { case (row, y) =>
          row.zipWithIndex.collect { case (RoundRock, x) => Position(x, y) }
        }
        .sortBy(position => (position.x, position.y))
        .toList

    def totalLoad: Long =
      gridArray.zipWithIndex.sumBy { case (row, y) =>
        row.count(_ == RoundRock) * (maxY - y)
      }

    def canMoveNorthOnce(pos: Position): Boolean = {
      val moved = pos.moveNorth
      pos.y > 0 && gridArray(moved.y)(moved.x) == EmptySpace
    }

    def canMoveSouthOnce(pos: Position): Boolean = {
      val moved = pos.moveSouth
      pos.y < maxY - 1 && gridArray(moved.y)(moved.x) == EmptySpace
    }

    def canMoveEastOnce(pos: Position): Boolean = {
      val moved = pos.moveEast
      pos.x < maxX - 1 && gridArray(moved.y)(moved.x) == EmptySpace
    }

    def canMoveWestOnce(pos: Position): Boolean = {
      val moved = pos.moveWest
      pos.x > 0 && gridArray(moved.y)(moved.x) == EmptySpace
    }

    def moveNorthOnce(pos: Position): Unit = {
      val moved = pos.moveNorth
      gridArray(moved.y)(moved.x) = gridArray(pos.y)(pos.x)
      gridArray(pos.y)(pos.x) = EmptySpace
    }

    def moveSouthOnce(pos: Position): Unit = {
      val moved = pos.moveSouth
      gridArray(moved.y)(moved.x) = gridArray(pos.y)(pos.x)
      gridArray(pos.y)(pos.x) = EmptySpace
    }

    def moveEastOnce(pos: Position): Unit = {
      val moved = pos.moveEast
      gridArray(moved.y)(moved.x) = gridArray(pos.y)(pos.x)
      gridArray(pos.y)(pos.x) = EmptySpace
    }

    def moveWestOnce(pos: Position): Unit = {
      val moved = pos.moveWest
      gridArray(moved.y)(moved.x) = gridArray(pos.y)(pos.x)
      gridArray(pos.y)(pos.x) = EmptySpace
    }

    def everyRoundRockMovesNorth(): Unit = {
      @tailrec
      def step(leftToMove: List[Position]): Unit =
        leftToMove match {
          case Nil =>
            ()

          case next :: tail if canMoveNorthOnce(next) =>
            moveNorthOnce(next)
            step(next.moveNorth :: tail)

          case _ :: tail =>
            step(tail)
        }

      step(roundRocksSortedByNorth())
    }

    def everyRoundRockMovesSouth(): Unit = {
      @tailrec
      def step(leftToMove: List[Position]): Unit =
        leftToMove match {
          case Nil =>
            ()

          case next :: tail if canMoveSouthOnce(next) =>
            moveSouthOnce(next)
            step(next.moveSouth :: tail)

          case _ :: tail =>
            step(tail)
        }

      step(roundRocksSortedBySouth())
    }

    def everyRoundRockMovesEast(): Unit = {
      @tailrec
      def step(leftToMove: List[Position]): Unit =
        leftToMove match {
          case Nil =>
            ()

          case next :: tail if canMoveEastOnce(next) =>
            moveEastOnce(next)
            step(next.moveEast :: tail)

          case _ :: tail =>
            step(tail)
        }

      step(roundRocksSortedByEast())
    }

    def everyRoundRockMovesWest(): Unit = {
      @tailrec
      def step(leftToMove: List[Position]): Unit =
        leftToMove match {
          case Nil =>
            ()

          case next :: tail if canMoveWestOnce(next) =>
            moveWestOnce(next)
            step(next.moveWest :: tail)

          case _ :: tail =>
            step(tail)
        }

      step(roundRocksSortedByWest())
    }

    def oneCycle(): Unit = {
      everyRoundRockMovesNorth()
      everyRoundRockMovesWest()
      everyRoundRockMovesSouth()
      everyRoundRockMovesEast()
    }

    def cycle(n: Long): Unit = {

      val cache: mutable.Map[String, Long] = mutable.Map.empty

      @tailrec
      def getCycleLength(left: Long): (Long, Long) =
        if (left <= 0) (0L, 0L)
        else {
          oneCycle()
          val cycleN = n - left + 1

          cache.get(str) match {
            case Some(cycleStart) =>
              (cycleN - cycleStart, cycleStart)

            case None =>
              cache.addOne(str -> cycleN)
              getCycleLength(left - 1L)
          }
        }

      val (cycleLength, cycleStart) = getCycleLength(n)

      val cycleLeft = (n - cycleStart) % cycleLength

      (0L until cycleLeft).foreach(_ => oneCycle())
    }
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day14/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Long = {
    val grid = parse(rows)
    grid.everyRoundRockMovesNorth()
    grid.totalLoad
  }

  def part2(rows: List[String]): Long = {
    val grid = parse(rows)
    grid.cycle(1000000000L)
    grid.totalLoad
  }

  def parse(rows: List[String]): Platform = {
    val grid: Array[Array[Space]] = rows.map { row =>
      row.toCharArray.collect {
        case char if char == EmptySpace.char => EmptySpace
        case char if char == RoundRock.char  => RoundRock
        case char if char == CubeRock.char   => CubeRock
      }: Array[Space]
    }.toArray

    Platform(grid)
  }
}
