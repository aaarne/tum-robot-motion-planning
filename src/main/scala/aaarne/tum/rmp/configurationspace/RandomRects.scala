package aaarne.tum.rmp.configurationspace

import aaarne.tum.rmp.geometry
import aaarne.tum.rmp.geometry.Rectangle
import breeze.linalg.DenseVector
import breeze.stats.distributions._

trait RandomRects {

  private val g = Uniform(0.1, 0.5)
  private val x = Uniform(-1, 1)
  private val y = Uniform(0, 1)

  def createRandomRects(amount: Int): Seq[Rectangle] =
    (0 until amount).map { _ =>
      geometry.Rectangle(DenseVector(x.sample(), y.sample()), g.sample(), g.sample())
    }.filterNot(_.doesPointCollide(DenseVector(0.0, 0.0)))

}
