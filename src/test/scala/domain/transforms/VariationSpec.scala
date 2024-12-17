package domain.transforms

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.math.*

class VariationSpec extends AnyWordSpec with Matchers {

  "Variation" should {
    "behave correctly with Linear variation" in {
      val point = Point(0.5, 0.5)
      Variation.Linear(point) shouldBe point
    }

    "behave correctly with Sinusoidal variation" in {
      val point = Point(0.5, 0.5)
      val expectedX = sin(0.5)
      val expectedY = sin(0.5)
      Variation.Sinusoidal(point) shouldBe Point(expectedX, expectedY)
    }

    "behave correctly with Spherical variation" in {
      val point = Point(0.5, 0.5)
      val r = sqrt(point.x * point.x + point.y * point.y)
      val expectedX = point.x / (r * r)
      val expectedY = point.y / (r * r)
      Variation.Spherical(point) shouldBe Point(expectedX, expectedY)
    }

    "behave correctly with Swirl variation" in {
      val point = Point(0.5, 0.5)
      val r = sqrt(point.x * point.x + point.y * point.y)
      val expectedX = point.x * sin(r * r) - point.y * cos(r * r)
      val expectedY = point.x * cos(r * r) + point.y * sin(r * r)
      Variation.Swirl(point) shouldBe Point(expectedX, expectedY)
    }

    "behave correctly with Horseshoe variation" in {
      val point = Point(0.5, 0.5)
      val r = sqrt(point.x * point.x + point.y * point.y)
      val expectedX = ((point.x + point.y) * (point.x - point.y)) / r
      val expectedY = (2 * point.x * point.y) / r
      Variation.Horseshoe(point) shouldBe Point(expectedX, expectedY)
    }

    "behave correctly with Polar variation" in {
      val point = Point(0.5, 0.5)
      val r = sqrt(point.x * point.x + point.y * point.y)
      val theta = atan2(point.y, point.x)
      val expectedX = theta / math.Pi
      val expectedY = r - 1
      Variation.Polar(point) shouldBe Point(expectedX, expectedY)
    }

    "behave correctly with Spiral variation" in {
      val point = Point(0.5, 0.5)
      val r = sqrt(point.x * point.x + point.y * point.y)
      val theta = atan2(point.y, point.x)
      val expectedX = (cos(theta) + sin(r)) / r
      val expectedY = (sin(theta) - cos(r)) / r
      Variation.Spiral(point) shouldBe Point(expectedX, expectedY)
    }

    "behave correctly with Fisheye variation" in {
      val point = Point(0.5, 0.5)
      val r = sqrt(point.x * point.x + point.y * point.y)
      Variation.Fisheye(point) shouldBe Point(r * point.y, r * point.x)
    }

    "behave correctly with Bubble variation" in {
      val point = Point(0.5, 0.5)
      val rSquared = point.x * point.x + point.y * point.y
      Variation.Bubble(point) shouldBe Point(
        4 * point.x / (rSquared + 4),
        4 * point.y / (rSquared + 4)
      )
    }
  }
}
