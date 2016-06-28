package graph

import scala.collection.mutable.ListBuffer

case class Graph(V: Int,E: Int, adj_list: IndexedSeq[Vector[UndirectedEdge]]) extends UndirectedGraph[UndirectedEdge] {

	override def degree(v: Int): Int = {
		require(v >= 0 & v < V, s"Specified vertex $v out of range [0, $V)")
		adj_list(v).length
	}

	override def adj(v: Int) = {
		require(v >= 0 & v < V, s"Specified vertex $v out of range [0, $V)")
		adj_list(v)
	}

	override def toString: String =  f"Undirected graph with $V%d vertices and $E%d edges"
}

object Graph {

	def apply(edgeList: Vector[(Int, Int)], allowDup: Boolean=false, allowSelf: Boolean=false):Graph = {

	  val V = edgeList.map(t => t._1 max t._2).max + 1

		val adj_init = Array.fill(V)(ListBuffer.empty[UndirectedEdge])

		if (allowDup) {
			var nedge = edgeList.length
			if (allowSelf) {
				edgeList foreach {
					t => {
						adj_init(t._1) += UndirectedEdge(t._1, t._2)
						if (t._1 != t._2) adj_init(t._2) += UndirectedEdge(t._2, t._1)
					}
				}
			} else {
				edgeList foreach {
					t =>
						if (t._1 != t._2) {
							adj_init(t._1) += UndirectedEdge(t._1, t._2)
							adj_init(t._2) += UndirectedEdge(t._2, t._1)
						} else nedge -= 1
				}
			}
			Graph(V, nedge, adj_init.map(_.toVector).toIndexedSeq)
		} else {
			val edgeSet =
				if (allowSelf)
					edgeList.map(t => (t._1 min t._2, t._1 max t._2)).toSet
				else
					edgeList.filter {
						t => t._1 != t._2
					}.map {
						t => (t._1 min t._2, t._1 max t._2)
					}.toSet

			edgeSet foreach {
				t => {
					adj_init(t._1) += UndirectedEdge(t._1, t._2)
					if (t._1 != t._2) adj_init(t._2) += UndirectedEdge(t._2, t._1)
				}
			}
			 Graph(V, edgeSet.size, adj_init.map(_.toVector).toIndexedSeq)
		}
	}
}