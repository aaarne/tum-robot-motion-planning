package tumrmp.visibility_graph

import tumrmp.geometry.{LineSegment, Polygon}

trait VisibilityGraph {

  val polygons: List[Polygon]

  type Edge = LineSegment

  lazy val visibilityGraph: List[Edge] =
    for {
      (poly1, ipoly1) <- polygons.zipWithIndex
      (poly2, ipoly2) <- polygons.zipWithIndex if ipoly1 < ipoly2
      (larger, smaller) = if (poly1.size >= poly2.size) (poly1, poly2) else (poly2, poly1)
      (v1, iv1) <- smaller.vertices.zipWithIndex
      (v2, iv2) <- larger.vertices.zipWithIndex if iv1 < iv2
      ls = LineSegment(v1, v2)
      if !(polygons exists (p => p.doesLineCollide(ls)))
    } yield ls

}
