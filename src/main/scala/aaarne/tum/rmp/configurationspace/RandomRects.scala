package aaarne.tum.rmp.configurationspace

import aaarne.tum.rmp.geometry.Rectangle
import breeze.linalg.DenseVector
import breeze.stats.distributions._

trait RandomRects {

  val widthHeightSampler: Rand[Double] = Uniform(0.1, 0.5)
  val xValueSampler: Rand[Double] = Uniform(-1, 1)
  val yValueSampler: Rand[Double] = Uniform(0, 1)

  def sampleRect: Rectangle = Rectangle(DenseVector(xValueSampler.sample(), yValueSampler.sample()),
    widthHeightSampler.sample(),
    widthHeightSampler.sample())

  private def rectValid(r: Rectangle): Boolean = !r.doesPointCollide(DenseVector(0.0, 0.0))

  def createRandomRects(amount: Int): Seq[Rectangle] = Stream.continually(sampleRect) filter rectValid take amount

}
