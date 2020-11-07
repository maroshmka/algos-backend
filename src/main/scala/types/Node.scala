package types

case class Node(x: Int, y: Int) {
  def canEqual(obj: Any): Boolean = obj.isInstanceOf[Node]

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: Node =>
        obj.canEqual(this) &&
          this.hashCode() == obj.hashCode()
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + x
    result = prime * result + y
    result
  }
}
