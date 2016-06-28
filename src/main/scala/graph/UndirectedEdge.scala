package graph

case class UndirectedEdge(u: Int, v: Int) extends EdgeLike {

	override def equals(other: Any) = other match {
		case that: UndirectedEdge => ((u == that.u) && (v == that.v)) ||
			((u == that.v) && (v == that.u))
		case _ => false
	}
	override def hashCode = {
		val minV = u min v
		val maxV = u max v
		41 * (minV + 41) + maxV
	}

	def reverse: UndirectedEdge = new UndirectedEdge(v, u)
	override def toString = s"$u <-> $v"
}

object UndirectedEdge {
	def apply(e: (Int, Int)) = new UndirectedEdge(e._1, e._2)
}