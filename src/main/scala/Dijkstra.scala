import scala.collection.mutable
import types._

object Dijkstra {
  val NOOP_NODE: Node = Node(-1, -1)

  def printPath(path: mutable.Map[Node, Node], end: Node): Unit = {
    if (end == NOOP_NODE) return
    printPath(path, path(end))
    println(end)
  }

  def dijkstraOOP(
      graph: Map[Node, List[Edge]],
      source: Node
  ): collection.mutable.Map[Node, Node] = {
    var q = mutable.Queue[Node]()
    q += source

    val distances = collection.mutable.Map() ++ graph map { case (k, _) =>
      k -> Int.MaxValue
    }
    val path = collection.mutable.Map() ++ graph map { case (k, _) =>
      k -> NOOP_NODE
    }
    path(source) = NOOP_NODE
    distances(source) = 0

    val visited = collection.mutable.Map() ++ graph map { case (k, _) =>
      k -> false
    }

    while (q.nonEmpty) {
      val curr = q.dequeue()
      graph(curr).foreach(edge => {
        var dst = edge.dst
        if (!visited(curr) && distances(dst) > edge.weight + distances(curr)) {
          distances(dst) = edge.weight + distances(curr)
          path(dst) = curr
        }
        if (!visited(dst)) q += dst
      })
      visited(curr) = true
    }

    path
  }

}
