import algorithms.MinSpanTree._
import graph.WeightedGraph
import java.io.{File, FileWriter}
import services.RobustMatcher
import services.utils._

object SceneGraphCreation {
	def main(args: Array[String]) {
		val photoDirectory = new File("photos").listFiles
		val imVec = for {
			i <- 1 to 3
			if i != 0
		} yield {
			val indx = photoDirectory.indexWhere(_ == new File("photos/pic_" + i + ".jpg"))
			loadOrExit(photoDirectory(indx))
		}
		val graphData = for {
			i <- imVec.indices
			j <- imVec.indices
			if i < j
		} yield {
			val robustMatcherInstance = new RobustMatcher()
			val matchData = robustMatcherInstance.matchImages(imVec(i), imVec(j))
			val matchGraphData = for {
				m <- matchData.matches
			} yield (((i + 1).toString + m.queryIdx.toString).toInt,
				((j + 1).toString + m.queryIdx.toString).toInt,
				m.distance.toDouble)
			matchGraphData
		}
		val sceneGraph = WeightedGraph(graphData.flatten.toVector)
	  val minSpanTree = KruskalMST(sceneGraph)
		val wr = new FileWriter( new File("graph_data_output.dot"))
		wr.write(sceneGraph.toString)
		wr.close()
	}
}