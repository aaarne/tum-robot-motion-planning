package aaarne.tum.rmp.visibilitygraph

import aaarne.tum.rmp.geometry.{LineSegment, Polygon}

trait VisibilityGraph {

  type Edge = LineSegment

  val polygons: List[Polygon]

  /**
    * This is just incredibly declarative :)
    */
  lazy val visibilityGraph: List[Edge] =
    for {
      (poly1, i) <- polygons.zipWithIndex
      (poly2, j) <- polygons.zipWithIndex
      if (poly1.size > poly2.size) || (poly1.size == poly2.size && j > i)
      (v1, k) <- poly1.vertices.zipWithIndex
      (v2, l) <- poly2.vertices.zipWithIndex if l > k
      if !(polygons exists (_.doesLineCollide(LineSegment(v1, v2))))
    } yield LineSegment(v1, v2)

}
