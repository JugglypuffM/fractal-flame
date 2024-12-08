package domain.transforms

import domain.image.Color

case class Affine(
    color: Color,
    a: Double,
    b: Double,
    c: Double,
    d: Double,
    e: Double,
    f: Double
) {
  def apply(point: Point): Point = {
    val newX = a * point.x + b * point.y + c
    val newY = d * point.x + e * point.y + f
    Point(newX, newY)
  }
}
