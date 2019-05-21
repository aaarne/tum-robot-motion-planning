package aaarne.tum.rmp.voronoi

import breeze.linalg._

trait VoronoiDiagram {

  val map: DenseMatrix[Boolean]

  implicit def bool2int(b: Boolean): Int = if (b) 1 else 0

  lazy val voronoi: DenseMatrix[Double] = {
    val startState = (1, map map bool2int, true)
    val endState = Stream.iterate(startState)(loop).filterNot(state => state._3).head

    println(s"Constructing the voronoi diagram took ${endState._1} iterations.")
    endState._2 map (_.toDouble)
  }

  private def loop(state: (Int, DenseMatrix[Int], Boolean)): (Int, DenseMatrix[Int], Boolean) = state match {
    case (index, m, _) =>
      var filled = false

      def indexMatches(pos: (Int, Int)): Int = pos match {
        case (x, y) => m(x, y) == index
      }

      for (x <- 1 to map.rows - 2; y <- 1 to map.cols - 2) {
        val count = List((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)).map(indexMatches).sum
        if ((count > 0) && (m(x, y) == 0)) {
          m(x, y) = index + 1
          filled = true
        }
      }
      (index + 1, m, filled)
  }


}

