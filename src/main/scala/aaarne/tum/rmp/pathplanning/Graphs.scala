package aaarne.tum.rmp.pathplanning

import breeze.linalg._
import breeze.numerics.abs

object Graphs {

  def bfs[Vertex](g: Map[Vertex, List[Vertex]], start: Vertex, goal: Vertex): Option[List[Vertex]] = {

    def extractPath(state: Vertex, transitions: Map[Vertex, Vertex]): List[Vertex] = {

      def loop(state: Vertex, path: List[Vertex]): List[Vertex] =
        if (state == start) path.reverse :+ goal
        else loop(transitions(state), path :+ transitions(state))

      loop(state, Nil)
    }


    def bfsrec(frontier: List[Vertex], explored: List[Vertex], transitions: Map[Vertex, Vertex]): Option[List[Vertex]] =
      frontier match {
        case Nil => None
        case head :: tail if head == goal => Some(extractPath(goal, transitions))
        case head :: tail =>
          val children = g(head) diff (tail ++ explored :+ head)
          bfsrec(tail ++ children, head :: explored, transitions ++ (children map (c => c -> head)))
      }

    bfsrec(start :: Nil, Nil, Map.empty)
  }

  def checkConnectivity[Vertex](g: Map[Vertex, List[Vertex]]): Int = {
    val n = g.size
    val laplacian: DenseMatrix[Double] = DenseMatrix.zeros(n, n)

    g.zipWithIndex foreach {
      case ((_, nbhd), i1) => nbhd.zipWithIndex foreach {
        case (_, i2) =>
          laplacian(i1, i2) = -1.0
      }
        laplacian(i1, i1) = 1.0 * nbhd.size
    }

    val eigvals = eig(laplacian).eigenvalues.valuesIterator.toList
    eigvals.count(p => abs(p) < 1e-6)
  }
}

