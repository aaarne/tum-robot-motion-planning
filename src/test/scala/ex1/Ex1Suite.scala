package ex1

import breeze.linalg._
import org.scalatest.{FlatSpec, Matchers}
import tumrmp.configuration_space.Robot
import tumrmp.geometry
import tumrmp.geometry.{LineSegment, Polygon, Rectangle}

class Ex1Suite extends FlatSpec with Matchers {

  trait TestEnvironment {
    val simpleRobot = new Robot(List(1, 1))
    val upright = List(0.5 * math.Pi, 0)
    val lying = List(0.0, 0.0)

    val longRobot = new Robot(List(1, 1, 1, 1))
    val wrapped = List(.5*math.Pi, .5*math.Pi, .5*math.Pi, .5*math.Pi)
    val zigzag = List(0, .5*math.Pi, -.5*math.Pi, .5*math.Pi)

    val rect = Rectangle(DenseVector(0.0, 0.0), 1.0, 1.0)

    val l1 = LineSegment(DenseVector(-1.0, 0), DenseVector(1.0, 0.0))
    val l2 = geometry.LineSegment(DenseVector(0, -1.0), DenseVector(0.0, 1.0))
    val l3 = geometry.LineSegment(DenseVector(2.0, 0.0), DenseVector(0.0, 2.0))
  }

  "The first frame" should "always be an identity matrix" in {
    new TestEnvironment {
      val frames = simpleRobot frames upright
      frames.head should be (DenseMatrix.eye[Double](3))
    }
  }

  "The upright position" should "be two meters high" in {
    new TestEnvironment {
      val frames = simpleRobot frames upright
      val expected = Matrix(
        (0.0, -1.0, 0.0),
        (1.0, 0.0, 2.0),
        (0.0, 0.0, 1.0)
      )
      val diff: Double = sum(frames(2) - expected)

      diff should be (0.0 +- 1e-6)
    }
  }

  "The lying position" should "have no y component" in {
    new TestEnvironment {
      val frames = simpleRobot frames lying
      val points = simpleRobot points lying

      frames(2)(0, 2) should be (2.0 +- 1e-6)
      frames(2)(1, 2) should be (0.0 +- 1e-6)
      points(2)(0) should be (2.0 +- 0.6)
      points(2)(1) should be (0.0 +- 1e-6)
    }
  }

  "The long robot" should "be at identity as well when wrapped" in {
    new TestEnvironment {
      val frames = longRobot frames wrapped
      val expected: Matrix[Double] = DenseMatrix.eye[Double](3)

      val diff = sum(frames.last - expected)
      diff should be (0.0 +- 1e-6)

    }
  }

  it should "be at (2, 2) for the zigzag pose" in new TestEnvironment {
    val lastpoint = (longRobot moveTo zigzag).points.last
    val diff = sum(lastpoint - Vector(2.0, 2.0))
    diff should be (0.0 +- 1e-6)
  }


  "Rectangle" should "collide at (0,0)" in {
    new TestEnvironment {
      rect doesPointCollide DenseVector(0.0, 0.0) should equal (true)
    }
  }

  it should "not collide at (1,0)" in {
    new TestEnvironment {
      rect doesPointCollide DenseVector(1.0, 0.0) should equal (false)
    }
  }

  it should "collide with line from (-1,-1) to (1, 1)" in new TestEnvironment {
    rect doesLineCollide geometry.LineSegment(DenseVector(-1.0, -1.0), DenseVector(1.0, 1.0))
  }

  it should "not collide with line from (-1, -1) to (-2, -2)" in new TestEnvironment {
    rect doesLineCollide geometry.LineSegment(DenseVector(-1.0, -1.0), DenseVector(-2.0, -2.0))
  }

  it should "not be complex" in new TestEnvironment {
    rect.complex should be (false)
  }

  "Line l1" should "intersect l2" in new TestEnvironment {
    l1 intersects l2 should equal (true)
  }

  it should "not intersect l3" in new TestEnvironment {
    l1 intersects l3 should equal (false)
  }

  it should "contain (0,0)" in new TestEnvironment {
    l1 onLine DenseVector(0.0, 0.0) should be (true)
  }

  it should "contain (1,0)" in new TestEnvironment {
    l1 onLine DenseVector(1.0, 0.0) should be (true)
  }

  it should "not contain (0,1)" in new TestEnvironment {
    l1 onLine DenseVector(0.0, 1.0) should be (false)
  }

  it should "be at distance 1.0 from (0,1)" in new TestEnvironment {
    l1 dist DenseVector(0.0, 1.0) should be (1.0 +- 1e-6)
  }

  it should "at at no distance from (1,0)" in new TestEnvironment {
    l1 dist DenseVector(1.0, 0.0) should be (0.0 +- 1e-6)
  }

  trait ConvexityEnvironment {
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
  }

  "None of the shapes" should "be complex" in new ConvexityEnvironment with TestEnvironment {
    convexPolygon.complex should be(false)
    concavePolygon.complex should be(false)
    rect.complex should be(false)
  }

  "Convexity" should "be detected on a convex polygon" in new ConvexityEnvironment {
    println(convexPolygon.complex)
    convexPolygon.convex should be(true)
  }

  it should "not be detected on a concave polygon" in new ConvexityEnvironment {
    concavePolygon.convex should be(false)
  }

  it should "be detected on a rectangle" in new TestEnvironment {
    rect.convex should be(true)
  }

}
