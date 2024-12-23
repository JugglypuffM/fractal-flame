package generation

import cats.effect.Async
import cats.effect.implicits.concurrentParTraverseOps
import cats.implicits.{
  catsSyntaxApplicativeId,
  toFlatMapOps,
  toFunctorOps,
  toTraverseOps
}
import domain.console.Config
import domain.image.Image.*
import domain.image.{Color, Image, Pixel}
import domain.transforms.{Point, Transform}

class ImageGenerator[F[_]: Async](config: Config, random: MyRandom[F]) {
  def randomBoundedPoint: F[Point] =
    for {
      x <- random.betweenDouble(config.xMin, config.xMax)
      y <- random.betweenDouble(config.yMin, config.yMax)
    } yield Point(x, y)

  def projectPoint(point: Point): F[Pixel] =
    val x =
      config.renderWidth - (((config.xMax - point.x) / (config.xMax - config.xMin)) * config.renderWidth).toInt - 1
    val y =
      config.renderHeight - (((config.yMax - point.y) / (config.yMax - config.yMin)) * config.renderHeight).toInt - 1
    Pixel(x, y).pure[F]

  def processRotation(
      point: Point,
      color: Color,
      image: ImageRefs[F]
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
      image: ImageRefs[F]
  ): F[Unit] = {
    def inner(currentPoint: Point, currentIteration: Int): F[Unit] =
      if (currentIteration >= config.iterations) {
        Async[F].unit
      } else
        for {
          affine <- random.elementOf(config.affines)
          variation <- random.elementOf(config.variations)
          newPoint <- Transform(affine, variation)(currentPoint)
          _ <- processRotation(newPoint, affine.color, image)
          _ <- inner(newPoint, currentIteration + 1)
        } yield ()

    inner(startPoint, 0)
  }

  def generateImage: F[ImageRefs[F]] =
    for {
      image <- Image.empty(config.renderWidth, config.renderHeight)

      randomPoints <- List
        .range(0, config.samples)
        .map(_ => randomBoundedPoint)
        .traverse(identity)

      _ <- randomPoints
        .parTraverseN(config.threads)(point => processIteration(point, image))
    } yield image

}
