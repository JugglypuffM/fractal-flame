package generation

import cats.effect.Async
import cats.effect.std.Random
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherId, toFlatMapOps, toFunctorOps}
import domain.console.Config
import domain.image.Image
import domain.image.Image.*
import domain.transforms.Point
import fs2.Stream

class ImageGenerator[F[_]: Async](config: Config, random: Random[F]) {
  private def randomBoundedPoint: F[Point] =
    for {
      x <- random.betweenDouble(config.xMin, config.xMax)
      y <- random.betweenDouble(config.yMin, config.yMax)
    } yield Point(x, y)

  def processIteration(
      point: Point,
      currentIteration: Int,
      image: Image[F]
  ): F[Unit] = {
    Async[F].tailRecM((point, currentIteration)) {
      case (currentPoint, iteration) =>
        if (iteration >= config.iterations) {
          ().asRight[(Point, Int)].pure[F]
        } else {
          for {
            transform <- random.elementOf(config.transforms)
            newPoint <- transform(currentPoint)
            x =
              config.width - (((config.xMax - newPoint.x) / (config.xMax - config.xMin)) * config.width).toInt - 1
            y =
              config.height - (((config.yMax - newPoint.y) / (config.yMax - config.yMin)) * config.height).toInt - 1
            _ <- image.updatePixel(x, y, transform.affine.color)
          } yield (newPoint, iteration + 1).asLeft[Unit]
        }
    }
  }

  def generateImage: F[Image[F]] =
    for {
      image <- Image.empty(config.width, config.height)

      _ <- Stream
        .range(0, config.samples)
        .covary[F]
        .parEvalMapUnordered(config.threads)(_ => randomBoundedPoint)
        .parEvalMapUnordered(config.threads)(processIteration(_, 0, image))
        .compile
        .drain
    } yield image

}
