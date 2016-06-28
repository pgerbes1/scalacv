package services

import services.utils._
import org.bytedeco.javacpp.indexer.{FloatIndexer, UByteRawIndexer}
import org.bytedeco.javacpp.opencv_calib3d._
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_features2d.{BFMatcher, ORB, Feature2D, AKAZE, KAZE}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class Result(matches: Array[DMatch],
                  keyPoints1: KeyPointVector,
                  keyPoints2: KeyPointVector,
                  fundamentalMatrix: Mat)

sealed trait CrossCheckType
case object NoCheck extends CrossCheckType
case object CrossCheck extends CrossCheckType
case object RatioCheck extends CrossCheckType
case object BothCheck extends CrossCheckType

class RobustMatcher(feature2D: Feature2D = KAZE.create(),
                    ratio: Float = 0.65f,
                    refineF: Boolean = true,
                    refineM: Boolean = true,
                    minDistanceToEpipolar: Double = 3.0,
                    confidenceLevel: Double = 0.99) {

	val normType: Int = NORM_L2

	def matchImages(image1: Mat, image2: Mat, crossCheckType: CrossCheckType = CrossCheck): Result = {
		val keyPoints1 = new KeyPointVector()
		val keyPoints2 = new KeyPointVector()
		feature2D.detect(image1, keyPoints1, new Mat())
		feature2D.detect(image2, keyPoints2, new Mat())

		val descriptors1 = new Mat()
		val descriptors2 = new Mat()
		feature2D.compute(image1, keyPoints1, descriptors1)
		feature2D.compute(image2, keyPoints2, descriptors2)

		val matcher = new BFMatcher(normType, crossCheckType == CrossCheck)

		val matches1 = new DMatchVectorVector()
		val matches2 = new DMatchVectorVector()

		if (crossCheckType == RatioCheck || crossCheckType == BothCheck) {
			matcher.knnMatch(descriptors1, descriptors2, matches1, 2)
			if (crossCheckType == BothCheck) {
				matcher.knnMatch(descriptors2, descriptors1, matches2, 2)
			}
		}
		val outputMatches: DMatchVector = crossCheckType match {
			case CrossCheck =>
				val r = new DMatchVector()
				matcher.`match`(descriptors1, descriptors2, r)
				r
			case RatioCheck =>
				val r = ratioTest(matches1)
				toDMatchVector(r)
			case BothCheck =>
				val r = ratioAndSymmetryTest(matches1, matches2)
				toDMatchVector(r)
			case NoCheck =>
				val r = new DMatchVector()
				matcher.`match`(descriptors1, descriptors2, r)
				r
		}
		val (refinedMatches, fundamentalMatrix) = ransacTest(outputMatches, keyPoints1, keyPoints2)
		Result(refinedMatches, keyPoints1, keyPoints2, fundamentalMatrix)
	}
	def ratioTest(matches: DMatchVectorVector): Array[DMatch] = {
		val destArray = ArrayBuffer[DMatch]()
		for (i <- 0 until matches.size.toInt) {
			val aMatch = matches.get(i)
			if (aMatch.size > 1) {
				if (aMatch.get(0).distance / aMatch.get(1).distance <= ratio) {
					destArray.append(aMatch.get(0))
				}
			}
		}
		destArray.toArray
	}
	def symmetryTest(matches1: Array[DMatch],
	                 matches2: Array[DMatch]): Array[DMatch] = {
		val destSeq = new ListBuffer[DMatch]()
		for (m1 <- matches1) {
			var break = false
			for (m2 <- matches2
			     if !break) {
				if (m1.queryIdx == m2.trainIdx && m2.queryIdx == m1.trainIdx) {
					destSeq += new DMatch(m1.queryIdx, m1.trainIdx, m1.distance)
					break = true
				}
			}
		}
		destSeq.toArray
	}
	def ratioAndSymmetryTest(matches1: DMatchVectorVector,
	                         matches2: DMatchVectorVector): Array[DMatch] = {

		val ratioMatches1 = ratioTest(matches1)
		val ratioMatches2 = ratioTest(matches2)
		val outputMatches = symmetryTest(ratioMatches1, ratioMatches2)
		outputMatches
	}
	def ransacTest(srcMatches: DMatchVector,
	               keyPoints1: KeyPointVector,
	               keyPoints2: KeyPointVector): (Array[DMatch], Mat) = {

		val (refinedMatches1, fundamentalMatrix) = {
			val (points1, points2) = toPoint2fVectorPair(srcMatches, keyPoints1, keyPoints2)
			val pointStatus = new Mat()
			val fundamentalMatrix = findFundamentalMat(
				toMat(points1),
				toMat(points2),
				pointStatus,
				FM_RANSAC,
				minDistanceToEpipolar,
				confidenceLevel)
			val outMatches = new ListBuffer[DMatch]()
			val pointStatusIndexer = pointStatus.createIndexer().asInstanceOf[UByteRawIndexer]
			for (i <- 0 until pointStatus.rows()) {
				val inlier = pointStatusIndexer.get(i) != 0
				if (inlier) {
					outMatches += srcMatches.get(i)
				}
			}
			(outMatches, fundamentalMatrix)
		}
		if (refineF || refineM)  {
			val (points1, points2) = toPoint2fVectorPair(toDMatchVector(refinedMatches1), keyPoints1, keyPoints2)
			val fundamentalMatrix = findFundamentalMat(
				toMat(points1),
				toMat(points2),
				FM_8POINT,
				minDistanceToEpipolar,
				confidenceLevel,
				null)
			if (refineM && fundamentalMatrix.total.toInt == 9 && !points1.isNull && !points2.isNull) {
				val newPoints1 = new Mat()
				val newPoints2 = new Mat()
				correctMatches(fundamentalMatrix,
					toMat(points1), toMat(points2),
					newPoints1, newPoints2)
				val newPoints1Indexer = newPoints1.createIndexer().asInstanceOf[FloatIndexer]
				val newPoints2Indexer = newPoints2.createIndexer().asInstanceOf[FloatIndexer]
				for (i <- 0 until points1.size.toInt) {
					val newPoint1x = newPoints1Indexer.get(0, i, 0)
					val newPoint1y = newPoints1Indexer.get(0, i, 1)
					val newPoint2x = newPoints2Indexer.get(0, i, 0)
					val newPoint2y = newPoints2Indexer.get(0, i, 1)

					keyPoints1.get(refinedMatches1(i).queryIdx).pt.x(newPoint1x)
					keyPoints1.get(refinedMatches1(i).queryIdx).pt.y(newPoint1y)
					keyPoints2.get(refinedMatches1(i).trainIdx).pt.x(newPoint2x)
					keyPoints2.get(refinedMatches1(i).trainIdx).pt.y(newPoint2y)
				}
			}
			(refinedMatches1.toArray, fundamentalMatrix)
		} else {
			(refinedMatches1.toArray, fundamentalMatrix)
		}
	}
}
