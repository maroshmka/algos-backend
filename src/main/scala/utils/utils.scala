package utils

import types.Node

object utils {
  def distance(src: Node, dst: Node): Int =
    Math.sqrt(Math.pow(dst.x - src.x, 2) + Math.pow(dst.y - src.y, 2)).toInt
}
