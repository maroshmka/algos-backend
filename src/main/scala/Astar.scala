import types.{Edge, Node}


object Astar {
  type Path = collection.mutable.Map[Node, Node]

  def diff(curr: Node, end: Node): Int = {
    end.id - curr.id
  }

  def astar(graph: Map[Node, List[Edge]], source: Node, end: Node, h: (Node, Node) => Int): Path = {
    var q = List[Node]()
    q = source :: q

    val distances = collection.mutable.Map() ++ graph map { case (k, _) => k -> 1000 }
    distances(source) = 0

    val path = collection.mutable.Map() ++ graph map { case (k, _) => k -> Node(-1) }
    path(source) = Node(-1)

    val fScore = collection.mutable.Map() ++ graph map { case (k, _) => k -> Int.MaxValue }
    fScore(source) = h(source, end)

    while (q.nonEmpty) {
      val curr = (fScore filterKeys q.toSet minBy (_._2))._1
      if (curr == end) return path

      // remove curr from queue
      q = q.filterNot(_ == curr)

      graph(curr).foreach(
        edge => {
          val dst = Node(edge.dst)

          if (distances(dst) > edge.weight + distances(curr)) {
            distances(dst) = edge.weight + distances(curr)
            fScore(dst) = distances(dst) + h(dst, end)
            path(dst) = curr

            if (!q.contains(dst)) q = dst :: q

          }
        }
      )
    }

    path
  }
}