import scala.collection.mutable
import types._

object Dijkstra {

  def printPath(path: mutable.Map[Node, Node], end: Node): Unit = {
    if (end == Node(-1)) return
    printPath(path, path(end))
    println(end)
  }

  def dijkstraOOP(graph: Map[Node, List[Edge]], source: Node): collection.mutable.Map[Node, Node] = {
    var q = mutable.Queue[Node]()
    q += source

    val distances = collection.mutable.Map() ++ graph map { case (k, _) => k -> 1000 }
    val path = collection.mutable.Map() ++ graph map { case (k, _) => k -> Node(-1) }
    path(source) = Node(-1)
    distances(source) = 0

    val visited = collection.mutable.Map() ++ graph map { case (k, _) => k -> false }

    while (q.nonEmpty) {
      val curr = q.dequeue()
      graph(curr).foreach(
        edge => {
          val dst = Node(edge.dst)
          if (!visited(curr) && distances(dst) > edge.weight + distances(curr)) {
            distances(dst) = edge.weight + distances(curr)
            path(dst) = curr
          }
          if (!visited(dst)) q += dst
        }
      )
      visited(curr) = true
    }

    path
  }

}
