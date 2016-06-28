package graph

class UnionFind(val V: Int, val compress: Boolean = false) {
	require(V > 0, s"Number of vertices $V must be positive")

	private[this] val comp = Range(0, V).toArray[Int]
	private[this] val compSize = Array.fill[Int](V)(1)
	private[this] var ncomp: Int = V

	def addEdge(u: Int, v: Int): Unit = {
		require(u >= 0 & u < V, s"Specified vertex $u out of range [0, $V)")
		require(v >= 0 & v < V, s"Specified vertex $v out of range [0, $V)")

		val uRoot = component(u)
		val vRoot = component(v)

		if (uRoot != vRoot) {
			if (compSize(uRoot) < compSize(vRoot)) {
				comp(uRoot) = vRoot
				compSize(vRoot) += compSize(uRoot)
			} else {
				comp(vRoot) = uRoot
				compSize(uRoot) += compSize(vRoot)
			}
			ncomp -= 1
		}
	}
	def component(u: Int): Int = {
		require(u >= 0 & u < V, s"Specified vertex $u out of range [0, $V)")
		if (u == comp(u)) {
			u
		} else {
			var compIdx = u
			if (compress) {
				while (compIdx != comp(compIdx)) compIdx = comp(compIdx)
				var tmp = u
				while (tmp != comp(tmp)) {
					comp(tmp) = compIdx
					tmp = comp(tmp)
				}
			} else {
				while (compIdx != comp(compIdx)) {
					comp(compIdx) = comp(comp(compIdx))
					compIdx = comp(compIdx)
				}
			}
			compIdx
		}
	}
	def nComponents: Int = ncomp
	def connected(u: Int, v:Int): Boolean = {
		require(u >= 0 & u < V, s"Specified vertex $u out of range [0, $V)")
		require(v >= 0 & v < V, s"Specified vertex $v out of range [0, $V)")
		component(u) == component(v)
	}
}
object UnionFind {
	def apply(vertices: Vector[(Int, Int)], compress: Boolean=false): UnionFind = {
		val V = vertices.map(x => x._1 max x._2).max + 1
		val ret = new UnionFind(V, compress)
		for (v <- vertices) ret.addEdge(v._1, v._2)
		ret
	}
}