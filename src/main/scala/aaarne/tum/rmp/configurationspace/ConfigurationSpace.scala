package aaarne.tum.rmp.configurationspace

import aaarne.tum.rmp.geometry.Polygon
import breeze.linalg._
import breeze.numerics.toRadians

trait ConfigurationSpace extends CollisionChecker {

  val obstacles: List[Polygon]
  val robot: Robot
  val ignoreTable = false

  lazy val confspace: DenseMatrix[Double] = {
    val q1 = toRadians(linspace(0, 180, 181))
    val q2 = toRadians(linspace(0, 359, 360))

    DenseMatrix.tabulate(q1.size, q2.size){
      case (i, j) => if (checkCollision(robot moveTo List(q1(i), q2(j)), obstacles, ignoreTable = ignoreTable)) 0.0 else 1.0
    }
  }
}

