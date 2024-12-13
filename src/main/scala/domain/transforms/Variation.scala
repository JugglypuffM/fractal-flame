package domain.transforms

import scala.math.*

sealed trait Variation {
  def apply(point: Point): Point
}

object Variation {
  val variationAmount = 17

  private def calculateR(point: Point): Double =
    math.sqrt(point.x * point.x + point.y * point.y)

  private def calculateTheta(point: Point): Double =
    math.atan2(point.y, point.x)

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
      val r = calculateR(point)
      Point(
        point.x * sin(r * r) - point.y * cos(r * r),
        point.x * cos(r * r) + point.y * sin(r * r)
      )
    }
  }

  case object Horseshoe extends Variation {
    override def apply(point: Point): Point =
      val r = calculateR(point)
      Point(
        ((point.x + point.y) * (point.x - point.y)) / r,
        (2 * point.x * point.y) / r
      )
  }

  case object Polar extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      val theta = calculateTheta(point)
      Point(theta / math.Pi, r - 1)
    }
  }

  case object Spiral extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      val theta = calculateTheta(point)
      Point(
        (math.cos(theta) + math.sin(r)) / r,
        (math.sin(theta) - math.cos(r)) / r
      )
    }
  }

  case object Handkerchief extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      val theta = calculateTheta(point)
      Point(
        r * math.sin(theta + r),
        r * math.cos(theta - r)
      )
    }
  }

  case object Heart extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      val theta = calculateTheta(point)
      Point(
        r * math.sin(theta * r),
        -r * math.cos(theta * r)
      )
    }
  }

  case object Disc extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      val theta = calculateTheta(point)
      Point(
        theta / math.Pi * math.sin(math.Pi * r),
        theta / math.Pi * math.cos(math.Pi * r)
      )
    }
  }

  case object Diamond extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      val theta = calculateTheta(point)
      Point(
        math.sin(theta) * math.cos(r),
        math.cos(theta) * math.sin(r)
      )
    }
  }

  case object Ex extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      val theta = calculateTheta(point)
      val p0 = math.sin(theta + r)
      val p1 = math.cos(theta - r)
      Point(
        r * (p0 * p0 * p0 + p1 * p1 * p1),
        r * (p0 * p0 * p0 - p1 * p1 * p1)
      )
    }
  }

  case object Julia extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      val theta = calculateTheta(point)
      val omega = (if (math.random() < 0.5) 1 else -1) * theta / 2
      Point(
        math.sqrt(r) * math.cos(omega),
        math.sqrt(r) * math.sin(omega)
      )
    }
  }

  case object Waves extends Variation {
    override def apply(point: Point): Point = {
      Point(
        point.x + 0.5 * math.sin(point.y),
        point.y + 0.5 * math.sin(point.x)
      )
    }
  }

  case object Fisheye extends Variation {
    override def apply(point: Point): Point = {
      val r = calculateR(point)
      Point(
        r * point.y,
        r * point.x
      )
    }
  }

  case object Bubble extends Variation {
    override def apply(point: Point): Point = {
      val rSquared = point.x * point.x + point.y * point.y
      Point(
        4 * point.x / (rSquared + 4),
        4 * point.y / (rSquared + 4)
      )
    }
  }

  case object Exponential extends Variation {
    override def apply(point: Point): Point = {
      val exp = math.exp(point.x - 1)
      Point(
        exp * math.cos(math.Pi * point.y),
        exp * math.sin(math.Pi * point.y)
      )
    }
  }

}
