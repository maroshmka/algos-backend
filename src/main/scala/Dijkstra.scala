import scala.collection.mutable

object Dijkstra {

  case class Node(id: Int)

  case class Edge(src: Int, dst: Int, weight: Int)

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

  def main(args: Array[String]): Unit = {
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

    val path = dijkstraOOP(graph, Node(1))
    printPath(path, Node(6))
  }

}
