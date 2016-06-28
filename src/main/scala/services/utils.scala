package services

import java.awt._
import java.awt.image.BufferedImage
import java.io.File
import javax.swing.JFrame

import models.{Point => P}
import org.bytedeco.javacpp.indexer.{DoubleRawIndexer, FloatIndexer, FloatRawIndexer}
import org.bytedeco.javacpp.opencv_core.{Point, _}
import org.bytedeco.javacpp.opencv_features2d.{BFMatcher, Feature2D}
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.opencv_imgproc._
import org.bytedeco.javacv.OpenCVFrameConverter.ToMat
import org.bytedeco.javacv.{CanvasFrame, FFmpegFrameGrabber, Java2DFrameConverter, OpenCVFrameConverter}

import scala.math.round

object utils {

	def loadAndShowOrExit(file: File, flags: Int = IMREAD_COLOR): Mat = {
		val image = loadOrExit(file, flags)
		show(image, file.getName)
		image
	}

	def loadOrExit(file: File, flags: Int = IMREAD_COLOR): Mat = {
		val image = imread(file.getAbsolutePath, flags)
		if (image.empty()) {
			println("Couldn't load image: " + file.getAbsolutePath)
			sys.exit(1)
		}
		image
	}
	def show(mat: Mat, title: String) {
		val converter = new ToMat()
		val canvas = new CanvasFrame(title, 1)
		canvas.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
		canvas.showImage(converter.convert(mat))
	}

	def show(image: Image, title: String) {
		val canvas = new CanvasFrame(title, 1)
		canvas.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
		canvas.showImage(image)
	}

	def drawOnImage(image: Mat, points: Point2fVector): Mat = {
		val dest = image.clone()
		val radius = 5
		val red = new Scalar(0, 0, 255, 0)
		for (i <- 0 until points.size.toInt) {
			val p = points.get(i)
			circle(dest, new Point(round(p.x), round(p.y)), radius, red)
		}

		dest
	}

	def drawOnImage(image: Mat, overlay: Rect, color: Scalar): Mat = {
		val dest = image.clone()
		rectangle(dest, overlay, color)
		dest
	}

	def save(file: File, image: Mat) {
		imwrite(file.getAbsolutePath, image)
	}

	def toArray(keyPoints: KeyPoint): Array[KeyPoint] = {
		val oldPosition = keyPoints.position()
		// Convert keyPoints to Scala sequence
		val points = for (i <- Array.range(0, keyPoints.capacity.toInt)) yield new KeyPoint(keyPoints.position(i))
		// Reset position explicitly to avoid issues from other uses of this position-based container.
		keyPoints.position(oldPosition)

		points
	}

	def toArray(keyPoints: KeyPointVector): Array[KeyPoint] = {
		// for the simplicity of the implementation we will assume that number of key points is within Int range.
		require(keyPoints.size() <= Int.MaxValue)
		val n = keyPoints.size().toInt

		// Convert keyPoints to Scala sequence
		for (i <- Array.range(0, n)) yield new KeyPoint(keyPoints.get(i))
	}

	def toArray(matches: DMatchVector): Array[DMatch] = {
		// for the simplicity of the implementation we will assume that number of key points is within Int range.
		require(matches.size() <= Int.MaxValue)
		val n = matches.size().toInt

		// Convert keyPoints to Scala sequence
		for (i <- Array.range(0, n)) yield new DMatch(matches.get(i))
	}

	def toBufferedImage(mat: Mat): BufferedImage = {
		val openCVConverter = new ToMat()
		val java2DConverter = new Java2DFrameConverter()
		java2DConverter.convert(openCVConverter.convert(mat))
	}

	def toPoint(p: Point2f): Point = new Point(round(p.x), round(p.y))

	def toMat8U(src: Mat, doScaling: Boolean = true): Mat = {
		val min = Array(Double.MaxValue)
		val max = Array(Double.MinValue)
		minMaxLoc(src, min, max, null, null, new Mat())
		val (scale, offset) = if (doScaling) {
			val s = 255d / (max(0) - min(0))
			(s, -min(0) * s)
		} else (1d, 0d)

		val dest = new Mat()
		src.convertTo(dest, CV_8U, scale, offset)
		dest
	}

	def toMatPoint2f(points: Seq[Point2f]): Mat = {
		// Create Mat representing a vector of Points3f
		val dest = new Mat(1, points.size, CV_32FC2)
		val indx = dest.createIndexer().asInstanceOf[FloatIndexer]
		for (i <- points.indices) {
			val p = points(i)
			indx.put(0, i, 0, p.x)
			indx.put(0, i, 1, p.y)
		}
		require(dest.checkVector(2) >= 0)
		dest
	}

	def toMatPoint3f(points: Seq[Point3f]): Mat = {
		// Create Mat representing a vector of Points3f
		val dest = new Mat(1, points.size, CV_32FC3)
		val indx = dest.createIndexer().asInstanceOf[FloatIndexer]
		for (i <- points.indices) {
			val p = points(i)
			indx.put(0, i, 0, p.x)
			indx.put(0, i, 1, p.y)
			indx.put(0, i, 2, p.z)
		}
		dest
	}

	def toPoint2fArray(mat: Mat): Array[Point2f] = {
		require(mat.checkVector(2) >= 0, "Expecting a vector Mat")

		val indexer = mat.createIndexer().asInstanceOf[FloatIndexer]
		val size = mat.total.toInt
		val dest = new Array[Point2f](size)

		for (i <- 0 until size) dest(i) = new Point2f(indexer.get(0, i, 0), indexer.get(0, i, 1))
		dest
	}
	def toMat(points: Point2fVector): Mat = {
		val size: Int = points.size.toInt
		val dest = new Mat(1, size, CV_32FC2)
		val indx = dest.createIndexer().asInstanceOf[FloatIndexer]
		for (i <- 0 until size) {
			val p = points.get(i)
			indx.put(0, i, 0, p.x)
			indx.put(0, i, 1, p.y)
		}
		dest
	}
	def toVector(src: Array[DMatch]): DMatchVector = {
		val dest = new DMatchVector(src.length)
		for (i <- src.indices) dest.put(i, src(i))
		dest
	}
	def printInfo(mat: Mat, caption: String = ""): Unit = {
		println(
			caption + "\n" +
				s"  cols:     ${mat.cols}\n" +
				s"  rows:     ${mat.rows}\n" +
				s"  depth:    ${mat.depth}\n" +
				s"  channels: ${mat.channels}\n" +
				s"  type:     ${mat.`type`}\n" +
				s"  dims:     ${mat.dims}\n" +
				s"  total:    ${mat.total}\n"
		)
	}
	def makeImagesFromVideo(videoPath: String): Unit = {
		val frameGrabber = new FFmpegFrameGrabber(videoPath)
		frameGrabber.start()
		println(frameGrabber.getLengthInFrames.toString)
		for {
			i <- 0 until frameGrabber.getLengthInFrames
		} {
			frameGrabber.setFrameNumber(i)
			val conv = new OpenCVFrameConverter.ToMat()

			val f = conv.convert(frameGrabber.grab())
			println(i)
			save(new File("photos/pic_" + i + ".jpg"), f)
		}
		frameGrabber.stop()
	}
	def toPoint2fVectorPair(matches: DMatchVector,
	                        keyPoints1: KeyPointVector,
	                        keyPoints2: KeyPointVector): (Point2fVector, Point2fVector) = {
		val size = matches.size.toInt
		val pointIndexes1 = new Array[Int](size)
		val pointIndexes2 = new Array[Int](size)
		for (i <- 0 until size) {
			pointIndexes1(i) = matches.get(i).queryIdx()
			pointIndexes2(i) = matches.get(i).trainIdx()
		}
		val points1 = new Point2fVector()
		val points2 = new Point2fVector()
		KeyPoint.convert(keyPoints1, points1, pointIndexes1)
		KeyPoint.convert(keyPoints2, points2, pointIndexes2)

		(points1, points2)
	}

	def toDMatchVector(src: Seq[DMatch]): DMatchVector = {
		val dest = new DMatchVector(src.size)
		for ((m, i) <- src.toArray.zipWithIndex) {
			dest.put(i, m)
		}
		dest
	}

	def drawEpiLines(image: Mat, lines: Mat, points: Point2fVector): Mat = {
		val canvas = image.clone()
		val linesIndexer = lines.createIndexer().asInstanceOf[FloatIndexer]
		for (i <- 0 until lines.rows()) {
			val a = linesIndexer.get(i, 0, 0)
			val b = linesIndexer.get(i, 0, 1)
			val c = linesIndexer.get(i, 0, 2)
			val x0 = 0
			val y0 = math.round(-(c + a * x0) / b)
			val x1 = image.cols
			val y1 = math.round(-(c + a * x1) / b)
			line(canvas, new Point(x0, y0), new Point(x1, y1), new Scalar(255, 255, 255, 0), 1, LINE_AA, 0)
			val xp = math.round(points.get(i).x)
			val yp = math.round(points.get(i).y)
			val (color, width) = (new Scalar(0, 255, 255, 0), 1)
			circle(canvas, new Point(xp, yp), 3, color, width, LINE_AA, 0)
		}
		points.position(0)
		canvas
	}

	def getMatData(m: Mat) = for(i <- 0 until m.rows)
		yield for (j <- 0 until m.cols)
			yield m.createIndexer().asInstanceOf[FloatRawIndexer].get(i, j)

	def getPointData(m: Point2fVector) = for (i <- 0 until m.size.toInt) yield P(m.get(i).x, m.get(i).y)

	def matrixPrinter(m: Mat): Unit = {
		val idx = m.createIndexer().asInstanceOf[DoubleRawIndexer]
		for (j <- 0 until m.cols) {
			for (i <- 0 until m.rows) {
				println("%7.2f  ".format(idx.get(i, j)))
			}
			println("")
		}
	}

	def calcKeyPointsAndDescriptors(image: Mat, feature2D: Feature2D): (KeyPointVector, Mat) = {
		val keyPoints = new KeyPointVector()
		feature2D.detect(image, keyPoints, new Mat())
		val descriptors = new Mat()
		feature2D.compute(image, keyPoints, descriptors)
		(keyPoints, descriptors)
	}

	def matchImages(descrip1: Mat, descrip2: Mat): IndexedSeq[DMatchVector] = {
		val normType: Int = NORM_L2
		val matcher = new BFMatcher(normType)
		val matches = new DMatchVectorVector()
		val oneTwo = matcher.knnMatch(descrip1, descrip2, matches, 2)
		for( i <- 0 until matches.size.toInt) yield matches.get(i)
	 }

	}

