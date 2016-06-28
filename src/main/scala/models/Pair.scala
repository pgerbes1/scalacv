package models

case class Pair(point1: Point, point2: Point) {
  val distance: Double = point1.distance(point2)

  override def toString = point1 + "-" + point2 + " : " + distance
}

