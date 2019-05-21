package aaarne.tum.rmp.configurationspace

import aaarne.tum.rmp.geometry.Rectangle
import breeze.linalg.DenseVector
import breeze.stats.distributions._

trait RandomRects {

  private val g = Uniform(0.1, 0.5)
  private val x = Uniform(-1, 1)
  private val y = Uniform(0, 1)

  def sampleRect: Rectangle = Rectangle(DenseVector(x.sample(), y.sample()), g.sample(), g.sample())

  private def rectValid(r: Rectangle): Boolean = !r.doesPointCollide(DenseVector(0.0, 0.0))

  def createRandomRects(amount: Int): Seq[Rectangle] = Stream.continually(sampleRect) filter rectValid take amount

}
