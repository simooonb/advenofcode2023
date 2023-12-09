import Utils.ListOps

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Ordering.Implicits.seqOrdering

object Day8 {

  final case class Node(left: String, right: String)

  sealed trait Instruction
  case object Left  extends Instruction
  case object Right extends Instruction

  final case class Network(map: Map[String, Node]) {
    def move(from: String, instruction: Instruction): String =
      instruction match {
        case Left  => map(from).left
        case Right => map(from).right
      }
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day8/input.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part2(rows: List[String]): Long = {
    val (instructions, network) = parse(rows)

    val ids    = network.map.keys
    val starts = ids.filter(_.endsWith("A")).toList

    @tailrec
    def step(instructionsLeft: List[Instruction], current: String, steps: Int): Int =
      if (current.endsWith("Z"))
        steps
      else {
        val next :: tail = instructionsLeft
        val moveds       = network.move(current, next)
        val updatedTail  = if (tail.isEmpty) instructions else tail

        step(updatedTail, moveds, steps + 1)
      }

    val zs = starts.map(step(instructions, _, 0))
    lcm(zs.map(_.toLong))
  }

  // nicely copied from stack overflow
  def lcm(list: Seq[Long]): Long = list.foldLeft(1L) { (a, b) =>
    b * a /
      LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  def part1(rows: List[String]): Int = {
    val (instructions, network) = parse(rows)

    val start = "AAA"
    val end   = "ZZZ"

    @tailrec
    def step(instructionsLeft: List[Instruction], current: String, steps: Int): Int =
      if (current == end)
        steps
      else {
        val next :: tail = instructionsLeft
        val moved        = network.move(current, next)
        val updatedTail  = if (tail.isEmpty) instructions else tail

        step(updatedTail, moved, steps + 1)
      }

    step(instructions, start, 0)
  }

  def parse(rows: List[String]): (List[Instruction], Network) = {
    val moves :: tail = rows
    val nodeRegex     = "([A-Z0-9]{3}) = \\(([A-Z0-9]{3}), ([A-Z0-9]{3})\\)".r

    val instructions = moves.collect {
      case 'L' => Left
      case 'R' => Right
    }.toList

    val network =
      tail.collect { case nodeRegex(id, left, right) =>
        id -> Node(left, right)
      }.toMap

    instructions -> Network(network)
  }
}
