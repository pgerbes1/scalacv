package algorithms

import graph.{UnionFind, WeightedEdge, WeightedGraph}

import scala.collection.mutable.{ListBuffer, PriorityQueue => MPQueue}
import scala.math.Ordering

object MinSpanTree {
	val ord = new Ordering[WeightedEdge] {
		def compare(x: WeightedEdge, y: WeightedEdge): Int = y.weight compare x.weight
	}
	def LazyPrimMST(G: WeightedGraph): (Double, WeightedGraph) = {
		require(G.V > 0, "G has no vertices")

		def visit(G: WeightedGraph, u: Int, m: Array[Boolean],
		          pq: MPQueue[WeightedEdge]): Unit = {
			m(u) = true
			for (e <- G.adj(u))
				if (!m(e.v)) pq += e
		}

		val edges = new MPQueue[WeightedEdge]()(ord)
		val marked = Array.fill[Boolean](G.V)(false)
		val mst = new ListBuffer[(Int, Int, Double)]()

		visit(G, 0, marked, edges)
		while (edges.nonEmpty) {
			val e = edges.dequeue
			if (!(marked(e.u) && marked(e.v))) {
				mst += ((e.u, e.v, e.weight))
				if (!marked(e.u)) visit(G, e.u, marked, edges)
				if (!marked(e.v)) visit(G, e.v, marked, edges)
			}
		}
		val totwt = mst.foldLeft(0.0)(_ + _._3)
		(totwt, WeightedGraph(mst.toVector))
	}
	def KruskalMST(G: WeightedGraph): (Double, WeightedGraph) = {
		require(G.V > 0, "G has no vertices")

		val edges = new MPQueue[WeightedEdge]()(ord)
		val mst = new ListBuffer[(Int, Int, Double)]()
		val uf = new UnionFind(G.V, true)
		var nadded = 0

		edges ++= G.edges

		while (edges.nonEmpty && (nadded < G.V - 1)) {
			val e = edges.dequeue
			if (!uf.connected(e.u, e.v)) {
				uf.addEdge(e.u, e.v)
				mst += ((e.u, e.v, e.weight))
				nadded += 1
			}
		}
		val totwt = mst.foldLeft(0.0)(_ + _._3)
		(totwt, WeightedGraph(mst.toVector))
	}
}