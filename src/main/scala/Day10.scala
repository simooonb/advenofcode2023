import scala.Function.unlift
import scala.collection.mutable
import scala.io.Source

object Day10 {

  final case class Position(x: Int, y: Int)

  final case class Node(pos: Position, neighbours: List[Position]) {
    val id: String = s"${pos.x}-${pos.y}"
  }

  final case class Dirt(pos: Position, neighbours: List[Position]) {
    val id: String = s"dirt-${pos.x}-${pos.y}"
  }

  final case class Graph(nodes: Map[String, Node], dirts: List[Dirt]) {
    private lazy val dirtByPosition: Map[Position, Dirt] = dirts.map(d => d.pos -> d).toMap
    private lazy val nodeByPosition: Map[Position, Node] = nodes.values.groupBy(_.pos).view.mapValues(_.head).toMap
    private lazy val neighboursByNode: Map[String, List[Position]] = nodes.view.mapValues(_.neighbours).toMap
    private lazy val allPositions: Iterable[Position]              = nodes.values.map(_.pos) ++ dirts.map(_.pos)

    lazy val minX: Int = allPositions.minBy(_.x).x
    lazy val maxX: Int = allPositions.maxBy(_.x).x
    lazy val minY: Int = allPositions.minBy(_.y).y
    lazy val maxY: Int = allPositions.maxBy(_.y).y

    lazy val outsidePositions: List[Position] = {
      val topBot    = (minX to maxX).toList.map(x => List((x, minY - 1), (x, maxY + 1)))
      val leftRight = (minY to maxY).toList.map(y => List((minX - 1, y), (maxX + 1, y)))

      (topBot ++ leftRight).flatten.map(_.pos)
    }

    lazy val outsideDirts: List[Dirt] = outsidePositions.map(Dirt(_, Nil))

    def updateDirtsNeighbours(startOfCycle: Node): Graph = {
      val cycle          = cycleFrom(startOfCycle)
      val cyclePositions = cycle.map(_.pos).toSet

      val uselessPipesAsDirt = nodeByPosition.keys.toList
        .diff(cycle.map(_.pos))
        .map(Dirt(_, Nil))

      val allDirts = uselessPipesAsDirt ++ dirts ++ outsideDirts

      val dirtsWithNeighbours = allDirts.map { dirt =>
        val allPosAround = (for {
          xOffset <- -1 to 1
          yOffset <- -1 to 1
        } yield (dirt.pos.x + xOffset, dirt.pos.y + yOffset).pos)
          .filter(p => p != dirt.pos && dirtByPosition.contains(p))

        val neighbours = allPosAround.filter { pos =>
          !cyclePositions(pos) // todo: check for squeezing!!!!!!!
        }

        dirt.copy(neighbours = neighbours.toList)
      }

      println("dirts with neighbours")
      println(dirtsWithNeighbours)

      copy(dirts = dirtsWithNeighbours.distinct)
    }

    def floodedDirts: List[Dirt] = {
      val flooded    = mutable.Set.empty[Dirt]
      val floodQueue = new mutable.Queue[Dirt]()

      val outsideDirtsWithNeighbours = dirts.filter(d => d.pos.x < 0 || d.pos.y < 0)
      floodQueue.addAll(outsideDirtsWithNeighbours)

      println("all dirts")
      println(dirts)

      println(floodQueue)

      while (floodQueue.nonEmpty) {
        val next = floodQueue.dequeue()
        flooded.addOne(next)

        val toFlood = next.neighbours.map(dirtByPosition).collect { case neighbour if !flooded(neighbour) => neighbour }
        println(s"next: $next")
        println(s"next neighbours: ${next.neighbours}")
        println(s"toFlood: $toFlood")

        floodQueue.addAll(toFlood)
      }

      flooded.toList.diff(outsideDirts)
    }

    def neighbours(current: Node): List[Node] =
      neighboursByNode(current.id)
        .filterNot { neighbourPosCandidate =>
          neighboursByNode(nodeByPosition(neighbourPosCandidate).id).contains(current.id)
        }
        .map(nodeByPosition) // todo: collect

    def cycleFrom(start: Node): List[Node] = {
      val visited: mutable.Map[Node, Int] = mutable.Map.empty
      val finished: mutable.Set[Node]     = mutable.Set.empty

      def dfs(current: Node, position: Int): Unit = {
        if (finished(current)) ()
        else if (visited.contains(current)) ()
        else {
          visited(current) = position
          neighbours(current).foreach(dfs(_, position + 1))
          finished + current
        }
      }

      dfs(start, 1)

      visited.toList
        .sortBy { case (_, pos) => pos }
        .map { case (node, _) => node }
    }
  }

  // run with increased JVM stack size :lj:
  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day10/test-3.txt")
    val input  = source.getLines()
    val result = part2(input.toList)

    println(result)

    source.close()
  }

  def part2(rows: List[String]): Int = {
    val (graph, start)    = parse(rows)
    val updatedGraph      = graph.updateDirtsNeighbours(start)
    val floodedDirts      = updatedGraph.floodedDirts
    val unfloodedDirtsPos = updatedGraph.dirts.map(_.pos).diff(floodedDirts.map(_.pos))

    println(floodedDirts)
    println(unfloodedDirtsPos)
    println(unfloodedDirtsPos.size)
    unfloodedDirtsPos.size
  }

  def part1(rows: List[String]): Int = {
    val (graph, start) = parse(rows)
    val cycleGraph     = graph.cycleFrom(start)
    println(cycleGraph.map(_.pos))
    println(cycleGraph.size)
    println(cycleGraph.size / 2)
    cycleGraph.size / 2
  }

  implicit class TupleOps(t: (Int, Int)) {
    def pos: Position = Position(t._1, t._2)
  }

  def parse(rows: List[String]): (Graph, Node) = {
    val nodes = rows.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect {
        case ('|', x) => Node((x, y).pos, List((x, y - 1).pos, (x, y + 1).pos))
        case ('-', x) => Node((x, y).pos, List((x - 1, y).pos, (x + 1, y).pos))
        case ('L', x) => Node((x, y).pos, List((x, y - 1).pos, (x + 1, y).pos))
        case ('J', x) => Node((x, y).pos, List((x, y - 1).pos, (x - 1, y).pos))
        case ('7', x) => Node((x, y).pos, List((x, y + 1).pos, (x - 1, y).pos))
        case ('F', x) => Node((x, y).pos, List((x, y + 1).pos, (x + 1, y).pos))
      }
    }

    val emptyDirts = rows.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect { case ('.', x) =>
        Dirt((x, y).pos, Nil)
      }
    }

    def getStart(row: String, y: Int): Option[Position] =
      row.zipWithIndex.collectFirst {
        case (c, x) if c == 'S' => (x, y).pos
      }

    val startPos = rows.zipWithIndex.collectFirst(unlift((getStart _).tupled)).get

    // neighbours of start are those who are neighbours to him
    val startNeighbours = nodes.filter(_.neighbours.contains(startPos))
    val startNode       = Node(startPos, startNeighbours.map(_.pos))

    Graph((startNode :: nodes).map(n => n.id -> n).toMap, emptyDirts) -> startNode
  }
}
