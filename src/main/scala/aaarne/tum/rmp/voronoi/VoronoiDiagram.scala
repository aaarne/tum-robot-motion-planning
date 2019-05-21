package aaarne.tum.rmp.voronoi

import breeze.linalg._

trait VoronoiDiagram {

  val map: DenseMatrix[Boolean]

  lazy val voronoi: Matrix[Double] = Matrix.tabulate(map.rows, map.cols) {
    case (i, j) => 0.0
  }

}

