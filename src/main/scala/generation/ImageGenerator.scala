package generation

import cats.effect.Async
import cats.effect.implicits.concurrentParTraverseOps
import cats.effect.std.Random
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps, toFunctorOps}
import domain.console.Config
import domain.image.{Color, Pixel}
import domain.repository.ImageRepository
import domain.transforms.{Point, Transform}

class ImageGenerator[F[_]: Async](image: ImageRepository[F], config: Config, random: Random[F]) {
  private def randomBoundedPoint: F[Point] =
    for {
      x <- random.betweenDouble(config.xMin, config.xMax)
      y <- random.betweenDouble(config.yMin, config.yMax)
    } yield Point(x, y)

  private def projectPoint(point: Point): F[Pixel] =
    val x =
      config.width - (((config.xMax - point.x) / (config.xMax - config.xMin)) * config.width).toInt - 1
    val y =
      config.height - (((config.yMax - point.y) / (config.yMax - config.yMin)) * config.height).toInt - 1
    Pixel(x, y).pure[F]

  private def processRotation(
      point: Point,
      color: Color,
  ): F[Unit] =
    def inner(rotation: Int): F[Unit] =
      if (rotation >= config.symmetry) {
        Async[F].unit
      } else
        for {
          pixel <- projectPoint(
            point.rotate(rotation * (2 * math.Pi / config.symmetry))
          )
          _ <- image.updatePixel(pixel.x, pixel.y, color)
          _ <- inner(rotation + 1)
        } yield ()

    inner(0)

  def processIteration(
      startPoint: Point,
  ): F[Unit] = {
    def inner(currentPoint: Point, currentIteration: Int): F[Unit] =
      if (currentIteration >= config.iterations) {
        Async[F].unit
      } else
        for {
          affine <- random.elementOf(config.affines)
          variation <- random.elementOf(config.variations)
          newPoint <- Transform(affine, variation)(currentPoint)
          _ <- processRotation(newPoint, affine.color)
          _ <- inner(newPoint, currentIteration + 1)
        } yield ()

    inner(startPoint, 0)
  }

  def generateImage: F[Unit] =
    for {
      randomPoints <- List
        .range(0, config.samples)
        .map(_ => randomBoundedPoint)
        .parTraverseN(config.threads)(identity)
      _ <- randomPoints
        .parTraverseN(config.threads)(point => processIteration(point))
    } yield ()

}
