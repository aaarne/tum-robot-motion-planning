package aaarne.tum.rmp.visibilitygraph

import aaarne.tum.rmp.geometry.Polygon
import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions._


trait RandomPolygons {
  val positionSampling = Uniform(-5, 5)
  val radiusSampling = Uniform(0, 1)

  def sample(nVertices: Int): Polygon = {
    val uniform = Uniform(0, 1)

    val raw_angles = DenseVector[Double](uniform.sample(nVertices).toArray)
    val angles: Vector[Double] = 2 * math.Pi * accumulate(raw_angles) / sum(raw_angles)

    val origin = DenseVector(positionSampling.sample(2).toArray)
    val orientation = Uniform(0, math.Pi).sample()

    def createVertex(angle: Double): Vector[Double] =
      origin + DenseVector(cos(angle + orientation), sin(angle + orientation)) * radiusSampling.sample()

    Polygon((angles.valuesIterator map createVertex).toList)
  }

  def sampleConvexPolygons(n: Int, nVertices: Int): List[Polygon] =
    (Stream.continually(sample(nVertices)) filter (_.convex) take n).toList

  def sampleConcavePolygons(n: Int, nVertices: Int): List[Polygon] =
    (Stream.continually(sample(nVertices)) filterNot (_.convex) take n).toList

  def samplePolygon(maxVertices: Int = 10): Polygon = {
    val rnd = Rand.randInt(3, maxVertices + 1)
    sample(rnd.sample())
  }

  def sampleNonCollidingPolygon(polygons: List[Polygon], maxVertices: Int = 10): Polygon = {
    Stream.continually(samplePolygon(maxVertices)).filterNot {
      p => polygons exists (p1 => p1 collidesWith p)
    }.head
  }

  def removeCollidingPolygons(polys: List[Polygon]): List[Polygon] = {

    def loop(in: List[Polygon], acc: List[Polygon]): List[Polygon] = in match {
      case Nil => acc
      case poly :: rest if acc exists poly.collidesWith => loop(rest, acc)
      case poly :: rest => loop(rest, poly :: acc)
    }

    loop(polys, Nil)
  }

}
