package graph

trait GraphMutable[A <: EdgeLike] extends Mutable with Cloneable {

	def addEdge(edge: A): Unit

	def removeEdge(edge: A): Unit
}
