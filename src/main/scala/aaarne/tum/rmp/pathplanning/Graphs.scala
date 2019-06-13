package aaarne.tum.rmp.pathplanning

import breeze.linalg._
import breeze.numerics.abs

import scala.annotation.tailrec

object Graphs {

  /**
    * Perform breadth first search
    *
    * @param g     a graph in map from Vertex to List[Vertex] format
    * @param start start node
    * @param goal  end node
    * @return Shortest path solution if there is one, None otherwise
    */
  def bfs[Vertex](g: Map[Vertex, List[Vertex]], start: Vertex, goal: Vertex): Option[List[Vertex]] = {

    def extractPath(state: Vertex, transitions: Map[Vertex, Vertex]): List[Vertex] = {

      @tailrec def loop(state: Vertex, path: List[Vertex]): List[Vertex] =
        if (state == start) path.reverse :+ goal
        else loop(transitions(state), path :+ transitions(state))

      loop(state, Nil)
    }


    @tailrec def bfsrec(frontier: List[Vertex], explored: List[Vertex], transitions: Map[Vertex, Vertex]): Option[List[Vertex]] =
      frontier match {
        case Nil => None
        case head :: _ if head == goal => Some(extractPath(goal, transitions))
        case node :: tail =>
          val children = g(node) diff (tail ++ explored :+ node)
          bfsrec(tail ++ children, node :: explored, transitions ++ (children map (c => c -> node)))
      }

    bfsrec(start :: Nil, Nil, Map.empty)
  }

  /**
    * Compute number of connected components in the graph
    *
    * @param g the graph in map from Vertex to List[Vertex] format
    * @return Number of connected components 
    */
  def countCommunities(g: Map[_, List[_]]): Int = {
    val n = g.size
    val laplacian: DenseMatrix[Double] = DenseMatrix.zeros(n, n)

    val graphList = g.toList
    val graphKeys = graphList.map(_._1)

    graphList.zipWithIndex foreach {
      case ((_, nbhd), i1) =>
        (0 until n) foreach { i2 =>
          if (nbhd contains graphKeys(i2)) laplacian(i1, i2) = -1.0
        }
        laplacian(i1, i1) = 1.0 * nbhd.size
    }

    val eigvals = eig(laplacian).eigenvalues.valuesIterator.toList
    eigvals.count(p => abs(p) < 1e-6)
  }
}

