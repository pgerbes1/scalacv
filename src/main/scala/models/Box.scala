package models

import graph.Geom

case class Box(x: Double, y: Double, x2: Double, y2: Double) extends Geom {
	override def toBox: Box = this
}
object Box {
	val empty: Box = {
		val s = Math.sqrt(Double.MaxValue)
		val t = s + -2.0 * s
		Box(s, s, t, t)
	}
}