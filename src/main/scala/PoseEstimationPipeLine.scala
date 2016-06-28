import services.{CameraCalibration, RobustMatcher}
import java.io.{File, FileWriter}

import org.bytedeco.javacpp.opencv_calib3d._
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.indexer.{DoubleRawIndexer,UByteRawIndexer}
import services.utils._

object PoseEstimationPipeLine  {
	def main(args: Array[String]): Unit = {
				val fileList = for (i <- 1 to 8) yield new File("calibration/chessboard%02d.jpg".format(i))
				val cameraCalibrator = new CameraCalibration()
				val boardSize = new Size(6, 9)
				cameraCalibrator.addChessboardPoints(fileList, boardSize)
				val image = loadOrExit(fileList(6))
				cameraCalibrator.calibrate(image.size())

				val kMatrix = cameraCalibrator.cameraMatrix
				val distortion = cameraCalibrator.distortionCoeffs

				val photoDirectory = new File("photos").listFiles
				val imVec = for {
					i <- 1 to 3
					if i != 0
				} yield {
					val indx = photoDirectory.indexWhere(_ == new File("photos/pic_" + i + ".jpg"))
					loadOrExit(photoDirectory(indx))
				}
				val pointClouds = for {
					i <- imVec.indices
					j <- imVec.indices
					if i < j
				} yield {
					val robustMatcherInstance = new RobustMatcher()

					val matchData = robustMatcherInstance.matchImages(imVec(i), imVec(j))

					val fMatrix = matchData.fundamentalMatrix

					val eMatrix = multiply(multiply(kMatrix.t, fMatrix), kMatrix).asMat

					val (sourcePoints, targetPoints) = toPoint2fVectorPair(toVector(matchData.matches),
						matchData.keyPoints1,
						matchData.keyPoints2)

					val inliers = new Mat()
					val homography = findHomography(toMat(sourcePoints), toMat(targetPoints), inliers, RANSAC, 1.0)

					val numInliners = (for {i <- 0 until inliers.rows
					                        j <- 0 until inliers.cols
					} yield inliers.createIndexer().asInstanceOf[UByteRawIndexer].get(i, j)).sum

					val pMatrix = new Mat(3, 4, CV_64F, new Scalar(0f))
					val p = pMatrix.createIndexer.asInstanceOf[DoubleRawIndexer]
					        p.put(0, 0, 1f)
					        p.put(1, 1, 1f)
					        p.put(2, 2, 1f)


					val rotMatrix2 = new Mat()
					val translation2 = new Mat()
					val pose = recoverPose(eMatrix,
						toMat(sourcePoints),
						toMat(targetPoints),
						kMatrix,
						rotMatrix2,
						translation2)

					val pPrimeMatrix = new Mat(3, 4, CV_64F, new Scalar(0f))
					rotMatrix2.col(0).copyTo(pPrimeMatrix.col(0))
					rotMatrix2.col(1).copyTo(pPrimeMatrix.col(1))
					rotMatrix2.col(2).copyTo(pPrimeMatrix.col(2))
					translation2.col(0).copyTo(pPrimeMatrix.col(3))

					val hom3DPoints = new Mat()
					val triPoints = triangulatePoints(pMatrix, pPrimeMatrix, toMat(sourcePoints) , toMat(targetPoints), hom3DPoints)

					val wr = new FileWriter( new File("output_data.txt"))
					wr.write(getMatData(hom3DPoints.t.asMat).mkString("\n,"))
					wr.close()
					}
				}
	   }





