package aaarne.tum.rmp.visibilitygraph

import aaarne.tum.rmp.geometry.{LineSegment, Polygon}

trait VisibilityGraph {

  type Edge = LineSegment

  val polygons: List[Polygon]

  def checkEdge(edge: Edge): Boolean = !(polygons exists (_.lineCollides(edge)))

  /**
    * Generate each pair of polygons once, such that the polygon with more vertices is first
    */
  private def polygonPairs: List[(Polygon, Polygon)] =
    for {
      (poly1, i) <- polygons.zipWithIndex
      (poly2, j) <- polygons.zipWithIndex
      if i < j
    } yield if (poly1.size >= poly2.size) (poly1, poly2) else (poly2, poly1)

  lazy val visibilityGraph: List[Edge] =
    for {
      (p1, p2) <- polygonPairs
      (v1, i) <- p1.vertices.zipWithIndex
      (v2, j) <- p2.vertices.zipWithIndex
      e = LineSegment(v1, v2)
      if checkEdge(e)
    } yield e


}
