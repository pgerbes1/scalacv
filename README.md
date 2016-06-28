# scalacv

<p>A eclectic mix of code implementing topics in computer vision, graph theory and distributed computing.</p>

<p>The name stands for Scala Computer Vision as an homage to both OpenCV and javacv both of which are used heavily via the javacpp library.</p> 

<p>Currently there are 4 main example programs you can run:</p>

``` [1] PoseEstimationPipeLine
    [2] SceneGraphCreation
    [3] SparkFeatureMatch
    [4] VideoToPhotos
```

<p> The pose estimation, video processing and graph construction examples are run from the command line via **sbt run**.</p>

<p> The Spark option is not as it assumes you know how to package up a jar and submit the job to a cluster.</p>

<p>Finally, ***much*** credit and thanks goes to the following repos for their code and excellent examples.</p>  

https://github.com/bytedeco/javacv-examples

https://github.com/scala-graph/scala-graph