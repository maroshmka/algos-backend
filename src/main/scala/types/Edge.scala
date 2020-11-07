package types
import utils.utils

case class Edge(src: Node, dst: Node) {
  def weight(): Int = utils.distance(src, dst)
}
