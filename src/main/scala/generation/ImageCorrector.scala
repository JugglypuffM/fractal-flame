package generation

import cats.effect.Async
import cats.effect.implicits.concurrentParTraverseOps
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps, toFunctorOps}
import domain.console.Config
import domain.image.Image
import domain.image.Image.Image

class ImageCorrector[F[_]: Async](config: Config) {
  private def correctPixels(image: Image[F], maxHits: Int): F[Vector[Vector[Unit]]] =
    image
      .parTraverseN(config.threads)(
        _.parTraverseN(config.threads)(_.update(pixel =>
          pixel.copy(color = pixel.color.correct(pixel.hits, maxHits, config.gamma))
        ))
      )

  private def calculateMaxHits(image: Image[F]): F[Int] =
    image.foldLeft(0.pure[F])((max, row) =>
      row.foldLeft(max)((m, r) =>
        for {
          max <- m
          pixel <- r.get
        } yield math.max(max, pixel.hits)
      )
    )

  def logGammaCorrect(image: Image[F]): F[Unit] =
    for{
      maxHits <- calculateMaxHits(image)
      _ <- correctPixels(image, maxHits)
    } yield ()
    
  
}
