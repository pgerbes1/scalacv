package graph

import java.lang.Double.{isInfinite, isNaN}
import models.{Point, Box}
import scala.math.{max, min, sqrt}


trait Geom {
	def x: Double
	def y: Double
	def x2: Double
	def y2: Double
	def width: Double = x2 - x
	def height: Double = y2 - y

	def area: Double = width * height

	def distance(pt: Point): Double = sqrt(distanceSquared(pt))

	def distanceSquared(pt: Point): Double = {
		val dx = if (pt.x < x) x - pt.x else if (pt.x < x2) 0.0 else pt.x - x2
		val dy = if (pt.y < y) y - pt.y else if (pt.y < y2) 0.0 else pt.y - y2
		dx * dx + dy * dy
	}

	def isFinite: Boolean =
		!(isNaN(x) || isInfinite(x) ||
			isNaN(y) || isInfinite(y) ||
			isNaN(x2) || isInfinite(x2) ||
			isNaN(y2) || isInfinite(y2))

	def toBox: Box = Box(x, y, x2, y2)

	def lowerLeft: Point = Point(x, y)

	def upperRight: Point = Point(x2, y2)

	def contains(geom: Geom): Boolean = x <= geom.x && geom.x2 <= x2 && y <= geom.y && geom.y2 <= y2

	def intersects(geom: Geom): Boolean = x <= geom.x2 && geom.x <= x2 && y <= geom.y2 && geom.y <= y2

	def wraps(geom: Geom): Boolean = x < geom.x && geom.x2 < x2 && y < geom.y && geom.y2 < y2

	def expand(geom: Geom): Box = {
		val px1 = min(x, geom.x)
		val py1 = min(y, geom.y)
		val px2 = max(x2, geom.x2)
		val py2 = max(y2, geom.y2)
		Box(px1, py1, px2, py2)
	}

	def expandArea(geom: Geom): Double = {
		val px1 = min(x, geom.x)
		val py1 = min(y, geom.y)
		val px2 = max(x2, geom.x2)
		val py2 = max(y2, geom.y2)
		val a = (py2 - py1) * (px2 - px1)
		a - area
	}
}


