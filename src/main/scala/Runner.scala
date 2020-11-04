import Astar.astar
import Dijkstra._
import types._

object Runner {

  def makeGraph(): Map[Node, List[Edge]] = {
    val nodes = List(Node(1), Node(2), Node(3), Node(4), Node(5), Node(6))
    val edges = List(
      Edge(1, 2, 2),
      Edge(1, 5, 3),
      Edge(1, 4, 1),
      Edge(2, 3, 1),
      Edge(4, 5, 1),
      Edge(2, 5, 1),
      Edge(5, 6, 1),
      Edge(5, 1, 1),
      Edge(3, 6, 4)
    )
    val lookup = (x: Node) => edges filter (_.src == x.id)
    val graph = nodes map (x => x -> lookup(x)) toMap

    graph
  }

  def main(args: Array[String]): Unit = {
    val graph = makeGraph()
    val algo = astar _
    val printFn = printPath _
    val start = Node(1)
    val end = Node(6)

    // some heuristic, diff in 1d
    val h = (a: Node, b: Node) => b.id - a.id

    // todo - make strategy
    val path = algo(graph, start, end, h)

    printFn(path, end)

  }
}