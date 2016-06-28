package graph

trait GraphLike[A <: EdgeLike] {
	def V: Int
	def E: Int
	def adj(u: Int): Seq[A]
}





