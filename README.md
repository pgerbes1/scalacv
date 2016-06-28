# Scalacv

<p>A eclectic mix of code implementing topics in computer vision, graph theory and distributed computing.</p>

<p>The name stands for Scala Computer Vision as an homage to both OpenCV and JavaCV both of which are used heavily via the javacpp library.</p> 

<p>Currently there are 4 main example programs you can run:</p>

<b>1)</b> PoseEstimationPipeLine

<b>2)</b> SceneGraphCreation

<b>3)</b> SparkFeatureMatch

<b>4)</b> VideoToPhotos

<p> The pose estimation, video processing and graph construction examples are run from the command line via <b>sbt run</b>.</p>

<p> The Spark option is not as it assumes you know how to package up a jar and submit the job to a cluster.</p>

<p>Finally, <b><em>much</em></b> credit and thanks goes to the following repos for their code and excellent examples.</p>  

https://github.com/bytedeco/javacv-examples

https://github.com/scala-graph/scala-graph
