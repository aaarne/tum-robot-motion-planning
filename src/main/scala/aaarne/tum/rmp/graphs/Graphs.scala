package aaarne.tum.rmp.graphs

import breeze.linalg.{DenseMatrix, eig}
import breeze.numerics.abs

import scala.annotation.tailrec

object Graphs {

  /**
    * Perform breadth first search
    *
    * @param g     a graph given by a function from Vertex to List[Vertex] (note: map compatible!)
    * @param start start node
    * @param goal  end node
    * @return Shortest path solution if there is one, None otherwise
    */
  def bfs[Vertex](g: Vertex => List[Vertex], start: Vertex, goal: Vertex): Option[List[Vertex]] = {

    @tailrec def bfsrec(frontier: List[Vertex], explored: List[Vertex], transitions: Map[Vertex, Vertex]): Option[List[Vertex]] =
      frontier match {
        case Nil => None
        case node :: tail if node == goal => Some(extractPath(start, goal, transitions))
        case node :: tail =>
          val children = g(node) diff (tail ++ explored :+ node)
          bfsrec(tail ++ children, node :: explored, transitions ++ (children map (c => c -> node)))
      }

    bfsrec(start :: Nil, Nil, Map.empty)
  }


  /**
    * Perform depth first search
    *
    * @param g     a graph given by a function from Vertex to List[Vertex] (note: map compatible!)
    * @param start start node
    * @param goal  end node
    * @return The DFS solution path or None if no solution
    */
  def dfs[Vertex](g: Vertex => List[Vertex], start: Vertex, goal: Vertex): Option[List[Vertex]] = {

    @tailrec def dfsrec(frontier: List[Vertex], explored: List[Vertex], transitions: Map[Vertex, Vertex]): Option[List[Vertex]] =
      frontier match {
        case Nil => None
        case node :: tail if node == goal => Some(extractPath(start, goal, transitions))
        case node :: tail =>
          val children = g(node) diff (node :: explored)
          dfsrec(children ::: tail, node :: explored, transitions ++ (children map (c => c -> node)))
      }

    dfsrec(start :: Nil, Nil, Map.empty)
  }

  /**
    * Perform uniform cost search
    *
    * @param g     a graph given by a function from Vertex to neighbors list as (Vertex, Cost) pair
    * @param start start node
    * @param goal  end node
    * @return The shortest path solution path or None if no solution
    */
  def ucs[Vertex](g: Vertex => List[(Vertex, Double)], start: Vertex, goal: Vertex): Option[List[Vertex]] =
    astar(g, (_: Vertex) => 0.0, start, goal)


  /**
    * Perform A* search
    *
    * @param g          a graph given by a function from Vertex to neighbors list as (Vertex, Cost) pair
    * @param heuristics the heuristics function
    * @param start      start node
    * @param goal       end node
    * @return The shortest path solution path or None if no solution
    * @return
    */
  def astar[Vertex](g: Vertex => List[(Vertex, Double)], heuristics: Vertex => Double, start: Vertex, goal: Vertex): Option[List[Vertex]] = {

    @tailrec def astarrec(frontier: List[(Vertex, Double)], explored: Map[Vertex, Double], transitions: Map[Vertex, Vertex]): Option[List[Vertex]] =
      frontier match {
        case Nil => None
        case (node, _) :: tail if node == goal => Some(extractPath(start, goal, transitions))
        case (node, _) :: tail =>
          val cost = transitions.get(node).map(explored).getOrElse(0.0)
          val children = (g(node) filterNot (c => (explored.keySet contains c._1) || (c._1 == node))) map {
            case (v, c) => (v, cost + c + heuristics(v))
          }

          val newTransitions: List[Vertex] = children.toMap.keys.toList diff tail.filterNot {
            case (v, c) => children.toMap.get(v) match {
              case None => false
              case Some(c1) => c1 < c
            }
          }.map(_._1)

          val updatedFrontier = (tail ++ children).groupBy(_._1).toList.map {
            case (v, cs) => (v, cs.map(_._2).min)
          }.sortBy(_._2)

          astarrec(updatedFrontier, explored + (node -> cost), transitions ++ newTransitions.map(v => v -> node))
      }

    astarrec((start, heuristics(start)) :: Nil, Map.empty, Map.empty)
  }

  /**
    * Traverse path of transition map
    *
    * @param from        start node of the path
    * @param to          goal node of the path
    * @param transitions all transitions made in graph search
    * @return the path from start to goal using the transitions in the map
    */
  private def extractPath[Vertex](from: Vertex, to: Vertex, transitions: Map[Vertex, Vertex]): List[Vertex] = {

    @tailrec def loop(v: Vertex, path: List[Vertex]): List[Vertex] =
      if (v == from) path.reverse :+ to
      else loop(transitions(v), path :+ transitions(v))

    loop(to, Nil)
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
