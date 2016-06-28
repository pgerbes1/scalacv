package graph

trait VertexVisitor[A <: EdgeLike] {

	def startVertex(u: Int, g: GraphLike[A]) = {}

	def discoverVertex(u: Int, g: GraphLike[A]) = {}

	def treeEdge(e: A, g: GraphLike[A]) = {}

	def finalizeVertex(u: Int, g: GraphLike[A]) = {}
}
trait dfsVisitor[A <: EdgeLike] extends VertexVisitor[A] {

	def backEdge(e: A, g: GraphLike[A]) = {}

	def crossEdge(e: A, g: GraphLike[A]) = {}
}

trait bfsVisitor[A <: EdgeLike] extends VertexVisitor[A] {
	def nonTreeEdge(e: A, g: GraphLike[A]) = {}
}

class VisitCount[A <: EdgeLike] extends VertexVisitor[A] {
	private[this] var n = 0

	override def discoverVertex(u: Int, g: GraphLike[A]) = n += 1
	def resetNVisited() = n = 0
	def getNVisited = n
}

class VisitList[A <: EdgeLike] extends VertexVisitor[A] {
	private[this] val visited = collection.mutable.MutableList[Int]()

	override def discoverVertex(u: Int, g: GraphLike[A]) = visited += u
	def order = visited.toList
}
class VertexVisited[A <: EdgeLike](val g: GraphLike[A]) extends VertexVisitor[A] {
	private[this] var marked = Array.fill(g.V)(false)

	override def discoverVertex(u: Int, g: GraphLike[A]) = {
		assert(u < marked.length, "Vertex index %d out of range".format(u))
		marked(u) = true
	}
	def reset(): Unit = marked = Array.fill(g.V)(false)
	def didVisit(u: Int): Boolean = marked(u)
	def getNVisited: Int = marked.count(_ == true)
	def visitList: List[Int] =
		marked.zipWithIndex.filter(_._1 == true).map(_._2).toList
	def allVisited: Boolean = marked forall (_ == true)
}