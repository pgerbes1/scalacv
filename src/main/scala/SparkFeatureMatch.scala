import org.apache.spark._
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_features2d.{BFMatcher, KAZE}
import org.bytedeco.javacpp.opencv_imgcodecs.imread
import services.utils._

object SparkFeatureMatch {
	def main(args: Array[String]) {
		val alg = KAZE.create
		def matchImages(descrip1: Mat, descrip2: Mat): (Long, Long) = {
			val normType: Int = NORM_L2
			val matcher = new BFMatcher(normType)

			val matches1 = new DMatchVectorVector()
			val matches2 = new DMatchVectorVector()

			val oneTwo = matcher.knnMatch(descrip1, descrip2, matches1, 2)
			val twoOne = matcher.knnMatch(descrip2, descrip1, matches2, 2)
			(matches1.size, matches2.size)
		}
		val conf = new SparkConf().setAppName("SparkFeatureMatch")
		val spark = new SparkContext(conf)
		val photoDirectory = spark.textFile("/Users/patrickgerbes/scalcv/photos", 4)
		val features = photoDirectory.map(x => calcKeyPointsAndDescriptors(imread(x), alg))
		features.saveAsTextFile("/Users/patrickgerbes/sparkImageTest.txt")
	}
}
