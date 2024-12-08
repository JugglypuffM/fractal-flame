package domain.transforms

import scala.math.*

sealed trait Variation {
  def apply(point: Point): Point
}

object Variation{
  case object Linear extends Variation {
    override def apply(point: Point): Point = point
  }

  case object Sinusoidal extends Variation {
    override def apply(point: Point): Point =
      Point(math.sin(point.x), math.sin(point.y))
  }

  case object Spherical extends Variation {
    override def apply(point: Point): Point = {
      val r = sqrt(point.x * point.x + point.y * point.y)
      Point(point.x / (r * r), point.y / (r * r))
    }
  }

  case object Swirl extends Variation {
    override def apply(point: Point): Point = {
      val r = sqrt(point.x * point.x + point.y * point.y)
      Point(
        point.x * sin(r * r) - point.y * cos(r * r),
        point.x * cos(r * r) + point.y * sin(r * r)
      )
    }
  }

  case object Horseshoe extends Variation {
    override def apply(point: Point): Point =
      val r = sqrt(point.x * point.x + point.y * point.y)
      Point(
        ((point.x + point.y) * (point.x - point.y)) / r,
        (2 * point.x * point.y) / r
      )
  }

}