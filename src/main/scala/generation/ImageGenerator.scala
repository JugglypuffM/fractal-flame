package generation

import cats.effect.std.Random
import cats.effect.{Async, Ref}
import cats.implicits.{toFlatMapOps, toFunctorOps}
import domain.console.Config
import domain.image.Image
import domain.transforms.Point
import fs2.Stream

class ImageGenerator[F[_]: Async](config: Config, random: Random[F]) {
  private def randomBoundedPoint: F[Point] =
    for {
      x <- random.betweenDouble(config.xMin, config.xMax)
      y <- random.betweenDouble(config.yMin, config.yMax)
    } yield Point(x, y)

  def processIteration(ref: Ref[F, Image]): F[Unit] =
    for {
      point <- randomBoundedPoint
      transform <- random.elementOf(config.transforms)
      newPoint <- transform(point)
      x =
        config.width - (((config.xMax - newPoint.x) / (config.xMax - config.xMin)) * config.width).toInt - 1
      y =
        config.height - (((config.yMax - newPoint.y) / (config.yMax - config.yMin)) * config.height).toInt - 1
      _ <- ref.update(_.updatePixel(x, y, transform.affine.color))
    } yield ()

  def generateImage: F[Image] =
    for {
      imageRef <- Ref.of[F, Image](Image.empty(config.width, config.height))

      _ <- Stream
        .range(0, config.iterations)
        .covary[F]
        .parEvalMapUnordered(config.threads)(_ => processIteration(imageRef))
        .compile
        .drain

      image <- imageRef.get
    } yield image

}
