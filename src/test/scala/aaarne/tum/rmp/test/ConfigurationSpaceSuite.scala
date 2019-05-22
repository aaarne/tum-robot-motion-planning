package aaarne.tum.rmp.test

import aaarne.tum.rmp.configurationspace.Robot
import breeze.linalg._
import org.scalatest.{FlatSpec, Matchers}

class ConfigurationSpaceSuite extends FlatSpec with Matchers {

  trait TestEnvironment {
    val simpleRobot = new Robot(List(1, 1))
    val upright = List(0.5 * math.Pi, 0)
    val lying = List(0.0, 0.0)

    val longRobot = new Robot(List(1, 1, 1, 1))
    val wrapped = List(.5 * math.Pi,.5 * math.Pi,.5 * math.Pi,.5 * math.Pi)
    val zigzag = List(0,.5 * math.Pi, -.5 * math.Pi,.5 * math.Pi)

  }

  "The first frame" should "always be an identity matrix" in new TestEnvironment {
    (simpleRobot moveTo upright).frames.head should be(DenseMatrix.eye[Double](3))
  }

  "The upright position" should "be two meters high" in new TestEnvironment {
    val frames = (simpleRobot moveTo upright).frames
    val expected = Matrix(
      (0.0, -1.0, 0.0),
      (1.0, 0.0, 2.0),
      (0.0, 0.0, 1.0)
    )
    val diff: Double = sum(frames(2) - expected)

    diff should be(0.0 +- 1e-6)
  }

  "The lying position" should "have no y component" in new TestEnvironment {
    val state = simpleRobot moveTo lying
    val frames = state.frames
    val points = state.points

    frames(2)(0, 2) should be(2.0 +- 1e-6)
    frames(2)(1, 2) should be(0.0 +- 1e-6)
    points(2)(0) should be(2.0 +- 0.6)
    points(2)(1) should be(0.0 +- 1e-6)
  }

  "The long robot" should "be at identity as well when wrapped" in new TestEnvironment {
    val state = longRobot moveTo wrapped

    val expected: Matrix[Double] = DenseMatrix.eye[Double](3)
    val diff = sum(state.frames.last - expected)

    diff should be(0.0 +- 1e-6)
  }

  it should "be at (2, 2) for the zigzag pose" in new TestEnvironment {
    val lastpoint = (longRobot moveTo zigzag).points.last
    val diff = sum(lastpoint - Vector(2.0, 2.0))
    diff should be(0.0 +- 1e-6)
  }
}
