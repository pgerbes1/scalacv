package graph

private object VertexSearchStatus extends Enumeration {
	type VertexSearchStatus = Value
	val Undiscovered = Value
	val Discovered = Value
	val Finished = Value
}

object GraphSearch {

	import VertexSearchStatus._
	import collection.mutable.{Stack => MStack, Queue => MQueue}

	private def dfsInnerDi[A <: EdgeLike](g: DirectedGraph[A],
	                                      u: Int, visitor: dfsVisitor[A],
	                                      visited: Array[VertexSearchStatus]): Unit = {

						visited(u) = Discovered
						visitor.discoverVertex(u, g)
						for (e <- g.adj(u)) {
							if (visited(e.v) == Undiscovered) {
								visitor.treeEdge(e, g)
								dfsInnerDi(g, e.v, visitor, visited)
							} else if (visited(e.v) == Discovered)
								visitor.backEdge(e, g)
							else
								visitor.crossEdge(e, g)
						}
						visitor.finalizeVertex(u, g)
						visited(u) = Finished
					}

	private def dfsInnerUn[A <: EdgeLike](g: UndirectedGraph[A], u: Int, w: Int,
	                                      visitor: dfsVisitor[A],
	                                      visited: Array[VertexSearchStatus]): Unit = {
		visited(u) = Discovered
		visitor.discoverVertex(u, g) // Pre-order
		for (e <- g.adj(u)) {
			if (visited(e.v) == Undiscovered) {
				visitor.treeEdge(e, g)
				dfsInnerUn(g, e.v, e.u, visitor, visited)
			} else if (e.v != w & visited(e.v) == Discovered)
				visitor.backEdge(e, g)
		}
		visitor.finalizeVertex(u, g)
		visited(u) = Finished
	}

	def dfsVisitVertex[A <: EdgeLike](g: GraphLike[A], u: Int,
	                                  visitor: dfsVisitor[A]): Unit = {
		require(g.V > 0, "Empty graph")
		require(u < g.V & u >= 0, s"Invalid start vertex $u")

		val visited = Array.fill(g.V)(Undiscovered)
		visitor.startVertex(u, g)
		g match {
			case undig: UndirectedGraph[A] => dfsInnerUn (undig, u, u, visitor, visited)
			case dig: DirectedGraph[A] => dfsInnerDi (dig, u, visitor, visited)
		}
	}

	def dfsVisitAll[A <: EdgeLike](g: GraphLike[A], visitor: dfsVisitor[A]): Unit = {
		require(g.V > 0, "Empty graph")
		val visited = Array.fill(g.V)(Undiscovered)
		for (u <- 0 until g.V)
			if (visited(u) == Undiscovered) {
				visitor.startVertex(u, g)
				g match {
					case undig: UndirectedGraph[A] => dfsInnerUn(undig, u, u, visitor, visited)
					case dig: DirectedGraph[A] => dfsInnerDi(dig, u, visitor, visited)
				}
			}
	}

	private def bfsInner[A <: EdgeLike](g: GraphLike[A], s: Int,
	                                    visitor: bfsVisitor[A],
	                                    visited: Array[VertexSearchStatus]): Unit = {

		val q = new MQueue[Int]

		visited(s) = Discovered
		visitor.discoverVertex(s, g)
		q += s
		while (q.nonEmpty) {
			val u = q.dequeue()
			for (e <- g.adj(u)) {
				if (visited(e.v) == Undiscovered) {
					visitor.treeEdge(e, g)
					visited(e.v) = Discovered
					visitor.discoverVertex(e.v, g)
					q += e.v
				}
				else
					visitor.nonTreeEdge(e, g)
			}
			visited(u) = Finished
		}
	}

	def bfsVisitVertex[A <: EdgeLike](g: GraphLike[A], u: Int, visitor: bfsVisitor[A]): Unit = {
		require(g.V > 0, "Empty graph")
		require(u < g.V & u >= 0, s"Invalid start vertex $u")

		val visited = Array.fill(g.V)(Undiscovered)
		visitor.startVertex(u, g)
		bfsInner(g, u, visitor, visited)
	}

	def bfsVisitAll[A <: EdgeLike](g: GraphLike[A], visitor: bfsVisitor[A]): Unit = {
		require(g.V > 0, "Empty graph")
		val visited = Array.fill(g.V)(Undiscovered)
		for (u <- 0 until g.V)
			if (visited(u) == Undiscovered) {
				visitor.startVertex(u, g)
				bfsInner(g, u, visitor, visited)
			}
	}

	def connectedToVertex[A <: EdgeLike](u: Int, g: GraphLike[A]): List[Int] = {
		val vdet = new VertexVisited[A](g) with dfsVisitor[A]

		dfsVisitVertex(g, u, vdet)
		vdet.visitList
	}

	private class ConnectedComponents[A <: EdgeLike](g: GraphLike[A])
		extends VertexVisitor[A] {
		private[this] var idx: Int = -1
		private[this] val comps = Array.fill[Int](g.V)(idx)

		override def startVertex(u: Int, g: GraphLike[A]) = idx += 1
		override def discoverVertex(u: Int, g: GraphLike[A]) = comps(u) = idx
		def components: IndexedSeq[Int] = comps.toIndexedSeq
	}

	def findConnectedComponents[A <: EdgeLike](g: UndirectedGraph[A]): IndexedSeq[Int] = {
		val vis = new ConnectedComponents[A](g) with dfsVisitor[A]
		dfsVisitAll(g, vis)
		vis.components
	}

	private class CycleDetector[A <: EdgeLike] extends dfsVisitor[A] {
		private[this] var cycle = false

		def reset() = cycle = false
		override def backEdge(u: A, g: GraphLike[A]) = cycle = true
		def hasCycle: Boolean = cycle
	}

	def detectCycle[A <: EdgeLike](g: GraphLike[A]): Boolean = {
		val cdet = new CycleDetector[A]
		dfsVisitAll(g, cdet)
		cdet.hasCycle
	}

	private class Path[A <: EdgeLike](g: GraphLike[A], initVertex: Int)
		extends VertexVisitor[A] {
		val V = g.V
		val startVertex = initVertex
		val edgeTo = Array.fill[Int](V)(V)
		edgeTo(initVertex) = initVertex

		override def treeEdge(e: A, g: GraphLike[A]) = edgeTo(e.v) = e.u

		def hasPathTo(u: Int): Boolean = edgeTo(u) < V

		def pathTo(u: Int): Option[List[Int]] = {
			if (u == startVertex) {
				Some(List(u))
			} else if (!hasPathTo(u)) {
				None
			} else {
				val s = new MStack[Int]
				var currVertex = u
				while (currVertex != startVertex) {
					s.push(currVertex)
					currVertex = edgeTo(currVertex)
				}
				s.push(startVertex)
				Some(s.toList)
			}
		}
	}

	def findDFSPathBetween[A <: EdgeLike](u: Int, v: Int,
	                                      g: GraphLike[A]): Option[List[Int]] = {
		val vis = new Path[A](g, u) with dfsVisitor[A]
		dfsVisitVertex(g, u, vis)
		vis.pathTo(v)
	}

	def findDFSPathsFrom[A <: EdgeLike](u: Int, g: GraphLike[A]): Map[Int, List[Int]] = {
		val vis = new Path[A](g, u) with dfsVisitor[A]
		dfsVisitVertex(g, u, vis)

		val ret = collection.mutable.Map.empty[Int, List[Int]]
		for (v <- 0 until g.V)
			vis.pathTo(v) match {
				case Some(pth) => ret += (v -> pth)
				case None =>
			}
		ret.toMap
	}

	def findBFSPathBetween[A <: EdgeLike](u: Int, v: Int,
	                                      g: GraphLike[A]): Option[List[Int]] = {
		val vis = new Path[A](g, u) with bfsVisitor[A]
		bfsVisitVertex(g, u, vis)
		vis.pathTo(v)
	}

	def findBFSPathsFrom[A <: EdgeLike](u: Int, g: GraphLike[A]): Map[Int, List[Int]] = {
		val vis = new Path[A](g, u) with bfsVisitor[A]
		bfsVisitVertex(g, u, vis)

		val ret = collection.mutable.Map.empty[Int, List[Int]]
		for (v <- 0 until g.V)
			vis.pathTo(v) match {
				case Some(pth) => ret += (v -> pth)
				case None =>
			}
		ret.toMap
	}
	private class TopologicalSortVisitor[A <: EdgeLike] extends dfsVisitor[A] {
		private[this] var cycle = false
		private[this] val topo = new MStack[Int]

		def reset() = {
			cycle = false
			topo.clear()
		}
		override def backEdge(e: A, g: GraphLike[A]) = cycle = true

		override def finalizeVertex(u: Int, g: GraphLike[A]) =
			if (!cycle) topo.push(u)

		def hasCycle: Boolean = cycle

		def topologicalSort: Option[List[Int]] =
			if (cycle) None else Some(topo.toList)
	}

	def topologicalSort[A <: EdgeLike](g: DirectedGraph[A]): Option[List[Int]] = {
		val vis = new TopologicalSortVisitor[A]
		dfsVisitAll(g, vis)
		vis.topologicalSort
	}

	private class ReversePostVisitor[A <: EdgeLike] extends dfsVisitor[A] {
		private[this] val revpost = new MStack[Int] // Holds sort
		override def finalizeVertex(u: Int, g: GraphLike[A]) = revpost.push(u)
		def reversePost: List[Int] = revpost.toList
	}

	def kosaruComponents[A <: EdgeLike](g: DirectedGraph[A]): IndexedSeq[Int] = {
		require(g.V > 0, "Empty graph")

		val vis = new ReversePostVisitor[A]
		dfsVisitAll(g.reverse, vis)
		val order = vis.reversePost

		val visC = new ConnectedComponents[A](g) with dfsVisitor[A]
		val visited = Array.fill(g.V)(Undiscovered)
		for (u <- order)
			if (visited(u) == Undiscovered) {
				visC.startVertex(u, g)
				dfsInnerDi(g, u, visC, visited)
			}
		visC.components
	}

	def tarajanComponents[A <: EdgeLike](g: DirectedGraph[A]): IndexedSeq[Int] = {
		require(g.V > 0, "Empty graph")

		val preorder = Array.fill(g.V)(-1)
		val low = Array.fill(g.V)(g.V)
		val cc = Array.fill(g.V)(g.V)
		var cnt0 = 0
		var cnt1 = 0
		val stck = new MStack[Int]

		def tarajanInner(u: Int): Unit = {
			preorder(u) = cnt0
			low(u) = cnt0
			var minVal = cnt0
			cnt0 += 1
			stck.push(u)
			for (e <- g.adj(u)) {
				if (preorder(e.v) == -1) tarajanInner(e.v)
				if (low(e.v) < minVal) minVal = low(e.v)
			}
			if (minVal < low(u)) {
				low(u) = minVal
			} else {
				var v = g.V
				do {
					v = stck.pop()
					cc(v) = cnt1
					low(v) = g.V
				} while (v != u)
				cnt1 += 1
			}
		}
		for (u <- 0 until g.V)
			if (preorder(u) == -1) tarajanInner(u)

		cc.toIndexedSeq
	}

	def findConnectedComponents[A <: EdgeLike](g: DirectedGraph[A]): IndexedSeq[Int] = {
		tarajanComponents(g)
	}

}