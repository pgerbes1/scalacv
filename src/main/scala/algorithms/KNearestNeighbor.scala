package algorithms

import models.Point
import scala.annotation.tailrec

object KNearestNeighbor {

	def minK(seq: IndexedSeq[Point], x: Point, k: Int) = {
		val dist = (c: Point) => c.distance(x)
		def sort(seq: IndexedSeq[Point]): IndexedSeq[Point] = seq.size match {
			case 0 | 1 => seq
			case size =>
				val (left, right) = seq.splitAt(size / 2)
				merge(sort(left), sort(right))
		}
		def merge(left: IndexedSeq[Point], right: IndexedSeq[Point]) = {
			val leftF = left.lift
			val rightF = right.lift
			val builder = IndexedSeq.newBuilder[Point]
			@tailrec
			def loop(leftIndex: Int = 0, rightIndex: Int = 0): Unit = {
				if (leftIndex + rightIndex < k) {
					(leftF(leftIndex), rightF(rightIndex)) match {
						case (Some(leftCoord), Some(rightCoord)) =>
							if (dist(leftCoord) < dist(rightCoord)) {
								builder += leftCoord
								loop(leftIndex + 1, rightIndex)
							} else {
								builder += rightCoord
								loop(leftIndex, rightIndex + 1)
							}
						case (Some(leftCoord), None) =>
							builder += leftCoord
							loop(leftIndex + 1, rightIndex)

						case (None, Some(rightCoord)) =>
							builder += rightCoord
							loop(leftIndex, rightIndex + 1)

						case _ =>
					}
				}
			}
			loop()
			builder.result
		}
		sort(seq)
	}
}