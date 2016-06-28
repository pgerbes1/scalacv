package graph

trait EdgeLike {
	def u: Int
	def v: Int
	def isSelf: Boolean = u == v
	def reverse: EdgeLike
}


trait WeightedEdgeLike extends EdgeLike {
	def weight: Double
}

object EdgeImplicits {
	implicit def intTupleToUndirectedEdge(e: (Int, Int)): UndirectedEdge = UndirectedEdge(e)
	implicit def intTupleToDirectedEdge(e: (Int, Int)): DirectedEdge = DirectedEdge(e)
	implicit def intwtTupleToWeightedEdge(e: (Int, Int, Double)): WeightedEdge = WeightedEdge(e)
}

