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
      (poly2, j) <- polygons.zipWithIndex if j > i
      (larger, smaller) = if (poly1.size >= poly2.size) (poly1, poly2) else (poly2, poly1)

      (v1, k) <- smaller.vertices.zipWithIndex
      (v2, l) <- larger.vertices.zipWithIndex if l > k

      ls = LineSegment(v1, v2)
      if !(polygons exists (p => p.doesLineCollide(ls)))
    } yield ls

}
