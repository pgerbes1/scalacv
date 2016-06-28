package algorithms

import models.Point

object ClosestPair {

	def sortByX(points: Vector[Point]) = {
		points.sortBy(point => point.x)
	}

	def sortByY(points: Vector[Point]) = {
		points.sortBy(point => point.y)
	}

	def divideAndConquer(points: Vector[Point]): models.Pair = {
		val pointsSortedByX = sortByX(points)
		val pointsSortedByY = sortByY(points)

		divideAndConquer(pointsSortedByX, pointsSortedByY)
	}

	def bruteForce(points: Vector[Point]): models.Pair = {
		val numPoints = points.size
		if (numPoints < 2)
			return null
		var pair = models.Pair(points.head, points(1))
		if (numPoints > 2) {
			for (i <- 0 until numPoints - 1) {
				val point1 = points(i)
				for (j <- i + 1 until numPoints) {
					val point2 = points(j)
					val distance = point1.distance(point2)
					if (distance < pair.distance)
						pair = models.Pair(point1, point2)
				}
			}
		}
		pair
	}

	private def divideAndConquer(pointsSortedByX: Vector[Point], pointsSortedByY: Vector[Point]): models.Pair = {
		val numPoints = pointsSortedByX.size
		if (numPoints <= 3) {
			return bruteForce(pointsSortedByX)
		}

		val dividingIndex = numPoints >>> 1
		val leftOfCenter = pointsSortedByX.slice(0, dividingIndex)
		val rightOfCenter = pointsSortedByX.slice(dividingIndex, numPoints)

		var tempList = leftOfCenter.map(x => x)
		tempList = sortByY(tempList)
		var closestPair = divideAndConquer(leftOfCenter, tempList)

		tempList = rightOfCenter.map(x => x)
		tempList = sortByY(tempList)

		val closestPairRight = divideAndConquer(rightOfCenter, tempList)

		if (closestPairRight.distance < closestPair.distance)
			closestPair = closestPairRight

		tempList = Vector[Point]()
		val shortestDistance = closestPair.distance
		val centerX = rightOfCenter(0).x

		for (point <- pointsSortedByY) {
			if (Math.abs(centerX - point.x) < shortestDistance)
				tempList = tempList :+ point
		}

		closestPair = shortestDistanceF(tempList, shortestDistance, closestPair)
		closestPair
	}

	private def shortestDistanceF(tempList: Vector[Point], shortestDistance: Double, closestPair: models.Pair): models.Pair = {
		var shortest = shortestDistance
		var bestResult = closestPair
		for (i <- tempList.indices) {
			val point1 = tempList(i)
			for (j <- i + 1 until tempList.size) {
				val point2 = tempList(j)
				if ((point2.y - point1.y) >= shortestDistance)
					return closestPair
				val distance = point1.distance(point2)
				if (distance < closestPair.distance) {
					bestResult = models.Pair(point1, point2)
					shortest = distance
				}
			}
		}
		closestPair
	}
}
