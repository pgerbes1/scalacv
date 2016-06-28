package graph

import scala.math._

class WeightedEdge(val u: Int, val v: Int, val weight: Double)
	extends WeightedEdgeLike with EdgeLike {

	def canEqual(other: Any): Boolean = other.isInstanceOf[WeightedEdge]

	override def equals(other: Any) = other match {
		case that: WeightedEdge =>
			((u == that.u) && (v == that.v)) || ((u == that.v) && (v == that.u)) &&
				(abs(weight - that.weight) <= 0.0001)
		case _ => false
	}
	override def hashCode = {
		41 * (u.hashCode + v.hashCode) + weight.hashCode
	}

	def reverse: WeightedEdge = new WeightedEdge(v, u, weight)
	override def toString = s"""\"$u\" -- \"$v\" [color = blue, len = $weight];"""
}

object WeightedEdge {
	def apply(e: (Int, Int, Double)) = new WeightedEdge(e._1, e._2, e._3)
	def apply(f: Int, t: Int, w: Double) = new WeightedEdge(f, t, w)
}