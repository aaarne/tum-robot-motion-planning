package ex1

import breeze.linalg._
import ex1.configuration_space.{Rectangle, Robot}
import org.scalatest.{FlatSpec, Matchers}

class Ex1Suite extends FlatSpec with Matchers {

  trait TestEnvironment {
    val simpleRobot = new Robot(List(1, 1))
    val upright = List(0.5 * math.Pi, 0)
    val lying = List(0.0, 0.0)

    val longRobot = new Robot(List(1, 1, 1, 1))
    val wrapped = List(.5*math.Pi, .5*math.Pi, .5*math.Pi, .5*math.Pi)

    val rect = Rectangle(DenseVector(0.0, 0.0), 1.0, 1.0)
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

  "The wrapped robot" should "be at identity as well" in {
    new TestEnvironment {
      val frames = longRobot frames wrapped
      val expected: Matrix[Double] = DenseMatrix.eye[Double](3)

      val diff = sum(frames.last - expected)
      diff should be (0.0 +- 1e-6)

    }
  }

  "Collision with rectangle" should "appear at (0,0)" in {
    new TestEnvironment {
      rect doesPointCollide DenseVector(0.0, 0.0) should equal (true)
    }
  }

  it should "not appear at (1,0)" in {
    new TestEnvironment {
      rect doesPointCollide DenseVector(1.0, 0.0) should equal (false)
    }
  }

  it should "appear on line from (-1,-1) to (1, 1)" in new TestEnvironment {
    rect doesLineCollide(DenseVector(-1.0, -1.0), DenseVector(1.0, 1.0))
  }

  it should "not appear on line from (-1, -1) to (-2, -2)" in new TestEnvironment {
    rect doesLineCollide(DenseVector(-1.0, -1.0), DenseVector(-2.0, -2.0))
  }

}
