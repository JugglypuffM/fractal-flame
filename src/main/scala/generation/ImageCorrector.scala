package generation

import cats.effect.implicits.concurrentParTraverseOps
import cats.effect.{Async, Ref}
import cats.implicits.toFunctorOps
import domain.console.Config
import domain.image.Image.Image
import domain.image.{Image, Pixel}

class ImageCorrector[F[_]: Async](config: Config) {
  private def correctPixel(pixelRef: Ref[F, Pixel]): F[Unit] =
    pixelRef.update(pixel =>
      pixel.copy(color = pixel.color.correct(config.gamma))
    )

  def logGammaCorrect(image: Image[F]): F[Unit] =
    image.parTraverseN(config.threads)(_.parTraverseN(config.threads)(correctPixel)).as(())
}
