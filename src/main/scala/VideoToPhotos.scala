import java.io.File
import org.bytedeco.javacv.{FFmpegFrameGrabber, OpenCVFrameConverter}
import services.utils._

object VideoToPhotos {
	def main(args: Array[String]): Unit = {
		makeImagesFromVideo("videos/test_structure.mp4")
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
	}

}