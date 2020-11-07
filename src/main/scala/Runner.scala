import Astar.astar
import Dijkstra._
import types._
import utils.utils

object Runner {

  def makeGraph(): Map[Node, List[Edge]] = {
    val n =
      """1 1
        |2 1
        |5 1
        |3 3
        |2 5
        |5 4""".stripMargin
    val e =
      """1 1 2 1
        |1 1 2 5
        |2 1 3 3
        |2 5 3 3
        |3 3 5 1
        |3 3 5 4
        |2 5 5 4""".stripMargin

    // todo - try to handle invalid input :)
    val nodes =
      n.split("\n") map (_.split(" ") map (_.toInt)) map (x => Node(x(0), x(1)))
    val edges = e.split("\n") map (_.split(" ") map (_.toInt)) map (x =>
      Edge(Node(x(0), x(1)), Node(x(2), x(3)))
    ) toList
    val lookupEdges = (n: Node) => edges filter (_.src == n)
    val graph = nodes map (x => x -> lookupEdges(x)) toMap

    graph
  }

  def main(args: Array[String]): Unit = {
    val algo = Dijkstra.dijkstraOOP _
    val printFn = printPath _

    val graph = makeGraph()
    val start = Node(1, 1)
    val end = Node(5, 1)

    val path = algo(graph, start)
    printFn(path, end)

  }
}
