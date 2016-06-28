package graph

import scala.collection.mutable.ListBuffer

case class WeightedGraph(V: Int, E: Int, private val adj_list: IndexedSeq[Vector[WeightedEdge]])
	extends UndirectedGraph[WeightedEdge] {

	def degree(v: Int): Int = {
		require(v >= 0 & v < V, s"Specified vertex $v out of range [0, $V)")
		adj_list(v).length
	}
	def adj(v: Int) = {
		require(v >= 0 & v < V, s"Specified vertex $v out of range [0, $V)")
		adj_list(v)
	}
	def edges: Vector[WeightedEdge] = {
		val edgs = ListBuffer.empty[WeightedEdge]
		for (v <- Range(0, V))
			for (e <- adj(v))
				if (e.v > v) edgs += e
		edgs.toVector
	}
	override def toString: String = edges.mkString("\n")
}

object WeightedGraph {
	def apply(edgeList: Vector[(Int, Int, Double)],
	          allowDup: Boolean = false): WeightedGraph = {
		val V = edgeList.map(t => t._1 max t._2).max + 1
		val adj_init = Array.fill(V)(ListBuffer.empty[WeightedEdge])
		var nedge = edgeList.length
		if (allowDup) {
			edgeList foreach {
				t =>
					if (t._1 != t._2) {
						adj_init(t._1) += WeightedEdge(t._1, t._2, t._3)
						adj_init(t._2) += WeightedEdge(t._2, t._1, t._3)
					} else nedge -= 1
			}
		} else {
			val edgeSet = scala.collection.mutable.Set[(Int, Int)]()
			for (edg <- edgeList) {
				if (edg._1 != edg._2) {
					val edgeTup = (edg._1 min edg._2, edg._1 max edg._2)
					if (!edgeSet.contains(edgeTup)) {
						edgeSet += edgeTup
						adj_init(edg._1) += WeightedEdge(edg._1, edg._2, edg._3)
						adj_init(edg._2) += WeightedEdge(edg._2, edg._1, edg._3)
					}
				}
			}
			nedge = edgeSet.size
		}
		WeightedGraph(V, nedge, adj_init.map(_.toVector).toIndexedSeq)
	}
}