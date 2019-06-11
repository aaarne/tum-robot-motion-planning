package aaarne.tum.rmp.test

import aaarne.tum.rmp.geometry.{LineSegment, Polygon, Rectangle}
import breeze.linalg._
import breeze.numerics.sqrt
import org.scalatest.{FlatSpec, Matchers}

class GeometrySuite extends FlatSpec with Matchers {

  "Rectangle" should "collide at (0,0)" in new TestEnvironment {
    rect doesPointCollide DenseVector(0.0, 0.0) should equal(true)
  }

  it should "not collide at (1,0)" in new TestEnvironment {
    rect doesPointCollide DenseVector(1.0, 0.0) should equal(false)
  }

  it should "collide with line from (-1,-1) to (1, 1)" in new TestEnvironment {
    rect lineCollides LineSegment(DenseVector(-1.0, -1.0), DenseVector(1.0, 1.0))
  }

  it should "not collide with line from (-1, -1) to (-2, -2)" in new TestEnvironment {
    rect lineCollides LineSegment(DenseVector(-1.0, -1.0), DenseVector(-2.0, -2.0))
  }

  it should "not be complex" in new TestEnvironment {
    rect.complex should be(false)
  }

  "Line l1" should "intersect l2" in new TestEnvironment {
    l1 intersects l2 should equal(true)
  }

  it should "not intersect l3" in new TestEnvironment {
    l1 intersects l3 should equal(false)
  }

  it should "contain (0,0)" in new TestEnvironment {
    l1 onLine DenseVector(0.0, 0.0) should be(true)
  }

  it should "contain (1,0)" in new TestEnvironment {
    l1 onLine DenseVector(1.0, 0.0) should be(true)
  }

  it should "not contain (0,1)" in new TestEnvironment {
    l1 onLine DenseVector(0.0, 1.0) should be(false)
  }

  it should "be at distance 1.0 from (0,1)" in new TestEnvironment {
    l1 dist DenseVector(0.0, 1.0) should be(1.0 +- 1e-6)
  }

  it should "be at no distance from (1,0)" in new TestEnvironment {
    l1 dist DenseVector(1.0, 0.0) should be(0.0 +- 1e-6)
  }

  it should "be at distance sqrt(2) from (2, 1)" in new TestEnvironment {
    l1 dist DenseVector(2.0, 1.0) should be(sqrt(2) +- 1e-6)
  }

  "None of the shapes" should "be complex" in new TestEnvironment {
    convexPolygon.complex should be(false)
    concavePolygon.complex should be(false)
    rect.complex should be(false)
  }

  "Convexity" should "be detected on a convex polygon" in new TestEnvironment {
    convexPolygon.convex should be(true)
  }

  it should "not be detected on a concave polygon" in new TestEnvironment {
    concavePolygon.convex should be(false)
  }

  it should "be detected on a rectangle" in new TestEnvironment {
    rect.convex should be(true)
  }

  "(0.0, 0.1)" should "be contained in the convex polygon" in new TestEnvironment {
    convexPolygon contains testPoint should be(true)
  }

  it should "not be contained in the concave polygon" in new TestEnvironment {
    concavePolygon contains testPoint should be(false)
  }

  trait TestEnvironment {

    val rect = Rectangle(DenseVector(0.0, 0.0), 1.0, 1.0)

    val l1 = LineSegment(DenseVector(-1.0, 0), DenseVector(1.0, 0.0))
    val l2 = LineSegment(DenseVector(0, -1.0), DenseVector(0.0, 1.0))
    val l3 = LineSegment(DenseVector(2.0, 0.0), DenseVector(0.0, 2.0))

    val convexPolygon = new Polygon {
      override val vertices: List[Vector[Double]] = List(
        DenseVector(-1.0, 0.0),
        DenseVector(1.0, 0.0),
        DenseVector(1.0, 1.0),
        DenseVector(-1.0, 1.0)
      )
    }
    val concavePolygon = new Polygon {
      override val vertices: List[Vector[Double]] = List(
        DenseVector(-1.0, 0.0),
        DenseVector(0.0, 0.5),
        DenseVector(1.0, 0.0),
        DenseVector(1.0, 1.0),
        DenseVector(-1.0, 1.0)
      )
    }

    val testPoint = DenseVector(0.0, 0.1)
  }

}
