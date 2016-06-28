package graph

class DirectedEdge(val u: Int, val v: Int) extends EdgeLike {

	override def equals(other: Any) = other match {
		case that: DirectedEdge => (u == that.u) && (v == that.v)
		case _ => false
	}
	override def hashCode = {
		41 * (u + 41) + v
	}

	def reverse: DirectedEdge = new DirectedEdge(v, u)

	override def toString = s"$u -> $v"
}

object DirectedEdge {
	def apply(e: (Int, Int)) = new DirectedEdge(e._1, e._2)
	def apply(f: Int, t: Int) = new DirectedEdge(f, t)
}
