package graph

trait DirectedGraph[A <: EdgeLike] extends GraphLike[A] {

	def indegree(u: Int): Int

	def outdegree(u: Int): Int

	def reverse: DirectedGraph[A]
}

object DirectedGraph {

	def formatAsDot[A](graph: Set[(A, A)], attr: Option[A => Map[String, String]] = None): String = {
		val output = new StringBuilder()

		output.append(s"digraph exported {\n")

		val nodes = graph.flatMap { case (a, b) => Set(a, b) }

		nodes.foreach { node =>
			val a = attr match {
				case None => ""
				case Some(f) => f(node).map { case (k, v) => s"$k=$v" }.mkString(" ")
			}

			output.append(s"""\"$node\" [$a]\n""")
		}

		graph.foreach { case (source, dest) =>
			output.append(
				s"""\"$source\" -> \"$dest\"\n""")
		}

		output.append("}\n")
		output.toString()
	}
}