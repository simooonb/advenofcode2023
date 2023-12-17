import Utils._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day16 {

  sealed trait Mirror {
    def char: Char

    def reflect(beam: Beam): List[Beam]
  }

  case object Upward extends Mirror {
    val char: Char = '/'

    def reflect(beam: Beam): List[Beam] = {
      val newDirection = beam.direction match {
        case North => East
        case South => West
        case East  => North
        case West  => South
      }

      List(beam.copy(direction = newDirection))
    }
  }

  case object Downward extends Mirror {
    val char: Char = '\\'

    def reflect(beam: Beam): List[Beam] = {
      val newDirection = beam.direction match {
        case North => West
        case South => East
        case East  => South
        case West  => North
      }

      List(beam.copy(direction = newDirection))
    }
  }

  case object SplitterVertical extends Mirror {
    val char: Char = '|'

    def reflect(beam: Beam): List[Beam] = {
      val newDirections = beam.direction match {
        case North => List(North)
        case South => List(South)
        case East  => List(North, South)
        case West  => List(North, South)
      }

      newDirections.map(d => beam.copy(direction = d))
    }
  }

  case object SplitterHorizontal extends Mirror {
    val char: Char = '-'

    def reflect(beam: Beam): List[Beam] = {
      val newDirections = beam.direction match {
        case North => List(East, West)
        case South => List(East, West)
        case East  => List(East)
        case West  => List(West)
      }

      newDirections.map(d => beam.copy(direction = d))
    }
  }

  case object EmptySpace extends Mirror {
    val char: Char = '.'

    def reflect(beam: Beam): List[Beam] = List(beam)
  }

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East  extends Direction
  case object West  extends Direction

  final case class Position(x: Int, y: Int) {
    def moveSouth: Position = Position(x, y + 1)
    def moveNorth: Position = Position(x, y - 1)
    def moveEast: Position  = Position(x + 1, y)
    def moveWest: Position  = Position(x - 1, y)
  }

  final case class Beam(pos: Position, direction: Direction) {
    def moveOnce: Beam = direction match {
      case North => copy(pos = pos.moveNorth)
      case South => copy(pos = pos.moveSouth)
      case East  => copy(pos = pos.moveEast)
      case West  => copy(pos = pos.moveWest)
    }
  }

  final case class Grid(grid: Array[Array[Mirror]]) {
    val maxX: Int = grid.head.length
    val maxY: Int = grid.length

    def allStarterBeams: List[Beam] = {
      val maxXIndex = maxX - 1
      val maxYIndex = maxY - 1

      val goingSouthFromNorth = (0 until maxX).map { x =>
        val direction = grid(0)(x).reflect(Beam(Position(x, 0), South)).head.direction
        Beam(Position(x, 0), direction)
      }

      val goingNorthFromSouth = (0 until maxX).map { x =>
        val direction = grid(maxYIndex)(x).reflect(Beam(Position(x, maxYIndex), North)).head.direction
        Beam(Position(x, maxYIndex), direction)
      }

      val goingEastFromWest = (0 until maxY).map { y =>
        val direction = grid(y)(0).reflect(Beam(Position(0, y), East)).head.direction
        Beam(Position(0, y), direction)
      }

      val goingWestFromEast = (0 until maxY).map { y =>
        val direction = grid(y)(maxXIndex).reflect(Beam(Position(maxXIndex, y), West)).head.direction
        Beam(Position(maxXIndex, y), direction)
      }

      (goingSouthFromNorth ++ goingNorthFromSouth ++ goingEastFromWest ++ goingWestFromEast).toList
    }

    def str(energized: mutable.Set[Position]): String =
      grid.zipWithIndex
        .map { case (row, y) =>
          row.zipWithIndex
            .map { case (c, x) =>
              if (energized(Position(x, y))) '#' else c.char
            }
            .mkString("")
        }
        .mkString("\n")

    def rayTracing(starterBeam: Beam): mutable.Set[Position] = {
      val cache: mutable.Set[Beam]         = mutable.Set(starterBeam)
      val energized: mutable.Set[Position] = mutable.Set(starterBeam.pos)

      @tailrec
      def step(currentBeams: List[Beam]): Unit = {
        if (currentBeams.isEmpty)
          ()
        else {
          val newBeams =
            currentBeams.flatMap { beam =>
              val moved = beam.moveOnce

              if (cache(moved) || moved.pos.x < 0 || moved.pos.y < 0 || moved.pos.x >= maxX || moved.pos.y >= maxY)
                Nil
              else {
                energized.addOne(moved.pos)
                cache.addOne(moved)
                grid(moved.pos.y)(moved.pos.x).reflect(moved)
              }
            }

          step(newBeams)
        }
      }

      step(List(starterBeam))

      energized
    }
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day16/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part1(rows: List[String]): Int = {
    val grid             = parse(rows)
    val starterDirection = grid.grid(0)(0).reflect(Beam(Position(0, 0), East)).head.direction
    val starterBeam      = Beam(Position(0, 0), starterDirection)
    val energized        = grid.rayTracing(starterBeam)

    energized.size
  }

  def part2(rows: List[String]): Int = {
    val starterGrid = parse(rows)

    var maximumEnergized = 0

    starterGrid.allStarterBeams.foreach { starterBeam =>
      val grid      = parse(rows)
      val energized = grid.rayTracing(starterBeam)
      val result    = energized.size

      if (result > maximumEnergized)
        maximumEnergized = result
    }

    maximumEnergized
  }

  def parse(rows: List[String]): Grid = {
    val grid: Array[Array[Mirror]] = rows.map { row =>
      row.toCharArray.collect {
        case char if char == EmptySpace.char         => EmptySpace
        case char if char == Downward.char           => Downward
        case char if char == Upward.char             => Upward
        case char if char == SplitterVertical.char   => SplitterVertical
        case char if char == SplitterHorizontal.char => SplitterHorizontal
      }: Array[Mirror]
    }.toArray

    Grid(grid)
  }
}
