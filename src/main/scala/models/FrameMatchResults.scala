package models

import org.bytedeco.javacpp.opencv_core._

case class FrameMatchResults(graphData: Array[(Int, Int, Int, Int, Float)],
                             fMatrix: Mat,
                              hMatrix: Mat,
                             kp1: KeyPointVector,
                             kp2: KeyPointVector,
                             p1: Point2fVector,
                             p2: Point2fVector)


