package aaarne.tum.rmp.geometry

import java.awt.Color

import breeze.plot.Series

trait Plotter {

  def plot(color: Color, name: String = "")(shape: Shape): Series =
    plotClosedShape(shape.lineSegments, color, name)

  private def plotClosedShape(ls: List[LineSegment], color: Color, name: String): Series = {
    val x = ls.head.p1(0) :: (ls map (_.p2(0)))
    val y = ls.head.p1(1) :: (ls map (_.p2(1)))
    breeze.plot.plot(x, y, colorcode = s"${color.getRed},${color.getGreen},${color.getBlue}", name = name)
  }

  def plotLineSegments(ls: List[LineSegment], color: Color): List[Series] =
    ls map (l => plotClosedShape(List(l), color, ""))

}
