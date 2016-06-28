package graph

trait UndirectedGraph[A <: EdgeLike] extends GraphLike[A] {

	def degree(u: Int): Int
}