package ex1.configuration_space

import breeze.linalg._
import breeze.numerics.{cos, sin}


class Robot(linkLengths: List[Double]) {

  type Frame = Matrix[Double]

  def frames(jointAngles: List[Double]): List[Frame] = {

    case class Link(jointAngle: Double, linkLength: Double)

    def createTrafo(link: Link): Frame =
      link match {
        case Link(q, l) => Matrix(
            (cos(q), -sin(q), cos(q) * l),
            (sin(q), cos(q), sin(q) * l),
            (0.0, 0.0, 1.0)
          )
      }

    def frameRec(links: List[Link], previous: Frame): List[Frame] =
      links match {
        case Nil => previous :: Nil
        case link :: rest => previous :: frameRec(rest, previous * createTrafo(link))
      }

    val links = (jointAngles zip linkLengths) map {
      case (q, l) => Link(q, l)
    }

    frameRec(links, DenseMatrix.eye[Double](3))
  }

  def points(jointAngles: List[Double]): List[Vector[Double]] =
    frames(jointAngles).map(frame => DenseVector(frame(0, 2), frame(1, 2)))

  def moveTo(jointAngles: List[Double]): RobotState =
    RobotState(linkLengths, jointAngles)

}

case class RobotState(linkLengths: List[Double], jointAngles: List[Double]) extends Robot(linkLengths) {
  lazy val frames: List[Frame] = super.frames(jointAngles)
  lazy val points: List[Vector[Double]] = super.points(jointAngles)
}
