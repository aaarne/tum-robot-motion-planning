package tumrmp.configuration_space

import breeze.linalg._
import breeze.numerics.{cos, sin}
import tumrmp.geometry
import tumrmp.geometry.{LineSegment, Plottable}


class Robot(linkLengths: List[Double]) {

  type Frame = Matrix[Double]

  def points(jointAngles: List[Double]): List[Vector[Double]] = extractPoints(frames(jointAngles))

  def frames(jointAngles: List[Double]): List[Frame] = {

    def createTrafo(link: (Double, Double)): Frame =
      link match {
        case (q, l) => Matrix(
          (cos(q), -sin(q), cos(q) * l),
          (sin(q), cos(q), sin(q) * l),
          (0.0, 0.0, 1.0)
        )
      }

    def frameRec(links: List[(Double, Double)], acc: List[Frame]): List[Frame] =
      links match {
        case Nil => acc.reverse
        case link :: rest => frameRec(rest, acc.head * createTrafo(link) :: acc)
      }

    frameRec(jointAngles zip linkLengths, DenseMatrix.eye[Double](3) :: Nil)
  }

  protected def extractPoints(frames: List[Frame]): List[Vector[Double]] =
    frames map (frame => DenseVector(frame(0, 2), frame(1, 2)))

  def moveTo(jointAngles: List[Double]): RobotState = RobotState(linkLengths, jointAngles)

}

case class RobotState(linkLengths: List[Double], jointAngles: List[Double]) extends Robot(linkLengths) with Plottable {
  lazy val frames: List[Frame] = frames(jointAngles)
  lazy val points: List[Vector[Double]] = extractPoints(frames)
  lazy val lineSegments: List[LineSegment] = (points zip points.tail) map {
    case (p1, p2) => geometry.LineSegment(p1, p2)
  }
}
