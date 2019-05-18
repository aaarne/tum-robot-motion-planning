package ex1.configuration_space

import breeze.linalg._
import breeze.numerics.{cos, sin}


class Robot(linkLengths: List[Double]) {

  type Frame = Matrix[Double]

  private case class Link(jointAngle: Double, linkLength: Double)

  def frames(jointAngles: List[Double]): List[Frame] = {

    def frameRec(links: List[Link], previous: Frame): List[Frame] = links match {
      case Nil => previous :: Nil
      case Link(q, l) :: rest =>
        val T = Matrix(
          (cos(q), -sin(q), cos(q)*l),
          (sin(q),  cos(q), sin(q)*l),
          (0.0, 0.0, 1.0)
        )
        previous :: frameRec(rest, previous * T)
    }

    val links = (jointAngles zip linkLengths) map {
      case (q, l) => Link(q, l)
    }

    frameRec(links, DenseMatrix.eye[Double](3))

  }

  def points(jointAngles: List[Double]): List[Vector[Double]] =
    frames(jointAngles).map(frame => DenseVector(frame(0, 2), frame(1, 2)))

}
