package algorithms

object Dijkstra {

	type Path[Key] = (Double, Vector[Key])

	def Dijkstra[Key](lookup: Map[Key, Vector[(Double, Key)]],
	                  fringe: Vector[Path[Key]],
	                  dest: Key,
	                  visited: Set[Key]): Path[Key] = fringe match {
		case (dist, path) +: fringe_rest => path match {
			case key +: path_rest =>
				if (key == dest) (dist, path.reverse)
				else {
					val paths = lookup(key).flatMap {
						case (d, key) => if (!visited.contains(key)) Vector((dist + d, key +: path)) else Vector()
					}
					val sorted_fringe = (paths ++ fringe_rest).sortWith { case ((d1, _), (d2, _)) => d1 < d2 }
					Dijkstra(lookup, sorted_fringe, dest, visited + key)
				}
		}
		case Vector() => (0, Vector())
	}
}