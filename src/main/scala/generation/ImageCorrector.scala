package generation

import cats.effect.{Async, Ref}
import cats.implicits.{toFunctorOps, toFlatMapOps}
import domain.console.Config
import domain.image.{Image, Pixel}
import fs2.Stream

class ImageCorrector[F[_]: Async](config: Config) {
  def correctPixel(pixel: Pixel, ref: Ref[F, Image]): F[Unit] =
    val newColor = pixel.color.correct(config.gamma)
    ref.update(_.updatePixel(pixel.x, pixel.y, newColor))

  def logGammaCorrect(image: Image): F[Image] =
    for{
      imageRef <- Ref.of[F, Image](image)

      _ <- Stream
        .emits(image.grid)
        .flatMap(Stream.emits(_))
        .parEvalMapUnordered(config.threads)(pixel => correctPixel(pixel, imageRef))
        .compile
        .drain
      
      correctedImage <- imageRef.get
    } yield correctedImage

}
