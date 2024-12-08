package generation

import cats.effect.{Async, Ref}
import cats.implicits.{toFlatMapOps, toFunctorOps}
import domain.console.Config
import domain.image.Image.Image
import domain.image.{Image, Pixel}
import fs2.Stream

class ImageCorrector[F[_]: Async](config: Config) {
  private def correctPixel(pixelRef: Ref[F, Pixel]): F[Unit] =
    pixelRef.update(pixel =>
      pixel.copy(color = pixel.color.correct(config.gamma))
    )

  def logGammaCorrect(image: Image[F]): F[Unit] =
    Stream
      .emits(image)
      .flatMap(Stream.emits)
      .parEvalMapUnordered(config.threads)(correctPixel)
      .compile
      .drain
}
