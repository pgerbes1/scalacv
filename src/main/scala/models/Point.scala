package models

import java.lang.Double._

import graph.Geom

import scala.math._

case class Point(x: Double, y: Double) extends Geom {
	def x2: Double = x
	def y2: Double = y

	override def width: Double = 0.0
	override def height: Double = 0.0
	override def area: Double = 0.0
	override def lowerLeft: Point = this
	override def upperRight: Point = this

	override def distanceSquared(pt: Point): Double = {
		val dx = pt.x - x
		val dy = pt.y - y
		dx * dx + dy * dy
	}
  override def distance(pt: Point): Double = sqrt(distanceSquared(pt))

	override def isFinite: Boolean =
		!(isNaN(x) || isInfinite(x) || isNaN(y) || isInfinite(y))

	override def wraps(geom: Geom): Boolean = false
}

