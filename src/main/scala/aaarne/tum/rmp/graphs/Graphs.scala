package aaarne.tum.rmp.graphs

import breeze.linalg.{DenseMatrix, eig}
import breeze.numerics.abs

import scala.annotation.tailrec

trait GraphSearch {

  /**
    * Perform breadth first search
    *
    * @param g     a graph given by a function from Vertex to List[Vertex] (note: map compatible!)
    * @param start start node
    * @param goal  end node
    * @return Shortest path solution if there is one, None otherwise
    */
  def bfs[V](g: V => List[V], start: V, goal: V): Option[List[V]] = {

    @tailrec def bfsrec(frontier: List[V], explored: List[V], transitions: Map[V, V]): Option[List[V]] =
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
  def dfs[V](g: V => List[V], start: V, goal: V): Option[List[V]] = {

    @tailrec def dfsrec(frontier: List[V], explored: List[V], transitions: Map[V, V]): Option[List[V]] =
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
  def ucs[V, C](g: V => List[(V, C)], start: V, goal: V)(implicit num: Numeric[C]): Option[List[V]] =
    astar(g, (_: V) => num.zero, start, goal)


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
  def astar[V, C](g: V => List[(V, C)], heuristics: V => C, start: V, goal: V)(implicit num: Numeric[C]): Option[List[V]] = {
    import num._

    case class $(vertex: V, heuristicCost: C, pathCost: C)

    @tailrec def astarrec(frontier: List[$], explored: List[V], transitions: Map[V, V]): Option[List[V]] =
      frontier match {
        case Nil => None
        case $(node, _, _) :: tail if node == goal => Some(extractPath(start, goal, transitions))
        case $(node, _, pathCost) :: tail =>
          val children = (g(node) filterNot (c => (explored contains c._1) || (c._1 == node))) map {
            case (v, c) => $(v, pathCost + c + heuristics(v), pathCost + c)
          }

          val newTransitions: List[V] = children.map { case $(v, _, _) => v } diff tail.filterNot {
            case $(v, heuristicCost, _) => children.find { case $(n, _, _) => n == v } match {
              case None => false
              case Some($(_, heuristicCost1, _)) => heuristicCost1 < heuristicCost
            }
          }.map { case $(v, _, _) => v }

          val updatedFrontier = (tail ++ children).groupBy { case $(v, _, _) => v }.toList.map {
            case (v, states) => $(v, states.map { case $(_, hc, _) => hc }.min, states.map { case $(_, _, pc) => pc }.min)
          }.sortBy { case $(_, hc, _) => hc }

          astarrec(updatedFrontier, node :: explored, transitions ++ newTransitions.map(v => v -> node))
      }

    astarrec($(start, heuristics(start), zero) :: Nil, Nil, Map.empty)
  }

  /**
    * Traverse path of transition map
    *
    * @param from        start node of the path
    * @param to          goal node of the path
    * @param transitions all transitions made in graph search
    * @return the path from start to goal using the transitions in the map
    */
  private def extractPath[V](from: V, to: V, transitions: Map[V, V]): List[V] = {

    @tailrec def loop(v: V, path: List[V]): List[V] =
      if (v == from) path.reverse :+ to
      else loop(transitions(v), path :+ transitions(v))

    loop(to, Nil)
  }
}

trait GraphConnectivity {

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

object Graphs extends GraphSearch with GraphConnectivity
