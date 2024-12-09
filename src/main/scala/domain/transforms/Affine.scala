package domain.transforms

import domain.image.Color

import scala.util.Random

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

object Affine {
  def generateRandomAffine: Affine =
    Affine(
      Color(
        Random.between(0, 255),
        Random.between(0, 255),
        Random.between(0, 255)
      ),
      Random.nextDouble() * 3 - 1.5,
      Random.nextDouble() * 3 - 1.5,
      Random.nextDouble() * 3 - 1.5,
      Random.nextDouble() * 2 - 1,
      Random.nextDouble() * 2 - 1,
      Random.nextDouble() * 2 - 1
    )
}
