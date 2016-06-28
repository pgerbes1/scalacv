package services

import java.io.{File, IOException}

import utils._
import org.bytedeco.javacpp.opencv_calib3d._
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.opencv_imgproc
import org.bytedeco.javacpp.opencv_imgproc._
import org.bytedeco.javacv.CanvasFrame
import javax.swing.WindowConstants

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class CameraCalibration {

	private val objectPoints = new ArrayBuffer[Seq[Point3f]]
	private val imagePoints  = new ArrayBuffer[Mat]

	private val _cameraMatrix     = new Mat()
	private val _distortionCoeffs = new Mat()

	private var flag: Int = 0

	private var mustInitUndistort = true

	def cameraMatrix = _cameraMatrix.clone()

	def distortionCoeffs = _distortionCoeffs.clone()

	def setCalibrationFlag(radial8CoeffEnabled: Boolean, tangentialParamEnabled: Boolean) {
		flag = 0
		if (!tangentialParamEnabled) flag += CALIB_ZERO_TANGENT_DIST
		if (radial8CoeffEnabled) flag += CALIB_RATIONAL_MODEL
	}

	def addChessboardPoints(fileList: Seq[File], boardSize: Size): Int = {
		objectPoints.clear()
		imagePoints.clear()

		val objectCorners = for (i <- 0 until boardSize.height; j <- 0 until boardSize.width) yield new Point3f(i, j, 0)

		var successes = 0

		for (file <- fileList) {
			val image = imread(file.getAbsolutePath, IMREAD_GRAYSCALE)
			if (image == null) {
				throw new IOException("Couldn't load image: " + file.getAbsolutePath)
			}
			val imageCorners = new Mat()
			val found = findChessboardCorners(image, boardSize, imageCorners)
			cornerSubPix(
				image, imageCorners, new Size(5, 5), new Size(-1, -1),
				new TermCriteria(
					TermCriteria.MAX_ITER + TermCriteria.EPS,
					30,
					0.1)
			)
			if (imageCorners.size().area() == boardSize.area()) {
				imagePoints += imageCorners
				objectPoints += objectCorners
				successes += 1
			}
			drawChessboardCorners(image, boardSize, imageCorners, found)
			val canvas = new CanvasFrame("Corners on Chessboard: " + file.getName, 1)
			canvas.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
			canvas.showImage(toBufferedImage(image))

		}
		successes
	}
	def calibrate(imageSize: Size): Double = {
		mustInitUndistort = true
		val (objectPointsMatVect, imagePointsMatVect) = convertPoints()
		val rotationVectors = new MatVector()
		val translationVectors = new MatVector()
		calibrateCamera(
			objectPointsMatVect,
			imagePointsMatVect,
			imageSize,
			_cameraMatrix,
			_distortionCoeffs,
			rotationVectors, translationVectors,
			flag,
			new TermCriteria(TermCriteria.MAX_ITER + TermCriteria.EPS, 30, Double.MinPositiveValue)
		)
	}
	def remap(image: Mat): Mat = {
		val undistorted = new Mat()
		val map1 = new Mat()
		val map2 = new Mat()
		if (mustInitUndistort) {
			opencv_imgproc.initUndistortRectifyMap(
				_cameraMatrix,
				_distortionCoeffs,
				new Mat(),
				new Mat(),
				image.size(),
				CV_32FC1,
				map1, map2);
			mustInitUndistort = false
		}
		opencv_imgproc.remap(image, undistorted, map1, map2, INTER_LINEAR)
		undistorted
	}
	private def convertPoints(): (MatVector, MatVector) = {
		require(objectPoints.size == imagePoints.size, "Number of object and image points must match.")
		val objectPointsMatVect = new MatVector(objectPoints.size)
		val imagePointsMatVect = new MatVector(objectPoints.size)
		for (((objectP, imageP), i) <- objectPoints zip imagePoints zipWithIndex) {
			objectPointsMatVect.put(i, toMatPoint3f(objectP))
			imagePointsMatVect.put(i, imageP)
		}
		(objectPointsMatVect, imagePointsMatVect)
	}
}