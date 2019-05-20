package tumrmp.visibility_graph

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Uniform
import tumrmp.geometry.Polygon

trait PolygonSamplingSettings {
  val positionSampling = Uniform(-5, 5)
  val radiusSampling = Uniform(0, 1)
}

trait RandomPolygons extends PolygonSamplingSettings {

  def sample(nVertices: Int): Polygon = {
    val uniform = Uniform(0, 1)

    val raw_angles = DenseVector[Double](uniform.sample(nVertices).toArray)
    val angles: Vector[Double] = 2 * math.Pi * accumulate(raw_angles) / sum(raw_angles)

    val origin = DenseVector(positionSampling.sample(2).toArray)
    val orientation = Uniform(0, math.Pi).sample()

    def createVertex(angle: Double): Vector[Double] =
      origin + DenseVector(cos(angle + orientation), sin(angle + orientation)) * radiusSampling.sample()

    new Polygon {
      override val vertices: List[Vector[Double]] = (angles.valuesIterator map createVertex).toList
    }
  }

  def sampleConvexPolygons(n: Int, nVertices: Int): List[Polygon] =
    (Stream.continually(sample(nVertices)) filter (_.convex) take n).toList

  def sampleConcavePolygons(n: Int, nVertices: Int): List[Polygon] =
    (Stream.continually(sample(nVertices)) filterNot (_.convex) take n).toList

}
