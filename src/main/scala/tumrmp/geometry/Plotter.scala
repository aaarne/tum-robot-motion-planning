package tumrmp.geometry

import breeze.plot.Series

trait Plotter {

  def plot(shape: Shape, color: String, name: String): Series =
    plotClosedShape(shape.lineSegments, color, name)

  private def plotClosedShape(ls: List[LineSegment], color: String, name: String): Series = {
    val x = ls.head.p1(0) :: (ls map (_.p2(0)))
    val y = ls.head.p1(1) :: (ls map (_.p2(1)))
    breeze.plot.plot(x, y, colorcode = color, name = name)
  }

  def plotLineSegments(ls: List[LineSegment], color: String): List[Series] =
    ls map (l => plotClosedShape(List(l), color, ""))

}
