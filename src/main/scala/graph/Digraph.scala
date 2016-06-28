package graph

import scala.collection.mutable.ListBuffer

class Digraph(val V: Int, val E: Int,
              private val indeg: IndexedSeq[Int],
              private val adj_list: IndexedSeq[Vector[DirectedEdge]])
	extends DirectedGraph[DirectedEdge] {

	def outdegree(v: Int): Int = {
		require(v >= 0 & v < V, s"Specified vertex $v out of range [0, $V)")
		adj_list(v).length
	}

	def indegree(v: Int): Int = indeg(v)

	def adj(v: Int) = {
		require(v >= 0 & v < V, s"Specified vertex $v out of range [0, $V)")
		adj_list(v)
	}

	override def reverse: Digraph = {
		val ideg = adj_list.map(_.length)
		val adj_init = Array.fill(V)(ListBuffer.empty[DirectedEdge])
		for (u <- 0 until V; e <- adj_list(u)) adj_init(e.v) += e.reverse
		new Digraph(V, E, ideg, adj_init.map(_.toVector).toIndexedSeq)
	}

	override def toString: String = f"Directed graph with $V%d vertices and $E%d edges"
}

object Digraph {
	def apply(edgeList: Vector[(Int, Int)], allowDup: Boolean=false, allowSelf: Boolean=true): Digraph = {

		val V = edgeList.map(t => t._1 max t._2).max + 1

		val adj_init = Array.fill(V)(ListBuffer.empty[DirectedEdge])
		val ideg = Array.fill(V)(0)
		var nedge = 0
		if (allowDup) {
			if (allowSelf) {
				edgeList foreach {
					t => {
						adj_init(t._1) += DirectedEdge(t._1, t._2)
						ideg(t._2) += 1
						nedge += 1
					}
				}
			} else {
				edgeList foreach {
					t => if (t._1 != t._2) {
						adj_init(t._1) += DirectedEdge(t._1, t._2)
						ideg(t._2) += 1
						nedge += 1
					}
				}
			}
			new Digraph(V, nedge, ideg, adj_init.map(_.toVector).toIndexedSeq)
		} else {
			val edgeSet =
				if (allowSelf)
					edgeList.toSet
				else
					edgeList.filter {
						t => t._1 != t._2
					}.toSet
			edgeSet foreach {
				t => {
					adj_init(t._1) += DirectedEdge(t._1, t._2)
					ideg(t._2) += 1
				}
			}
			new Digraph(V, edgeSet.size, ideg, adj_init.map(_.toVector).toIndexedSeq)
		}
	}
}