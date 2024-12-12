package generation

import cats.effect.Async
import cats.implicits.toFunctorOps
import domain.console.Config
import domain.image.Image.{Image, ImageOps, ImageRefs}
import domain.image.{Color, Image, Pixel}

class ImageCorrector[F[_]: Async](config: Config) {
  private def correctPixels(
      image: Image,
      maxHits: Int
  ): Image =
    image.map(
      _.map(pixel =>
        pixel.copy(color =
          pixel.color.correct(pixel.hits, maxHits, config.gamma)
        )
      )
    )

  private def calculateMaxHits(image: Image): Int =
    image.flatten.maxBy(_.hits).hits

  def compressImage(image: Image): Image =
    def sumColorsForUpperLeft(pixel: Pixel): (Int, Int, Int) =
      (pixel.y until pixel.y + config.samplingFactor)
        .map(i =>
          image(i)
            .slice(pixel.x, pixel.x + config.samplingFactor)
            .foldLeft((0, 0, 0))((acc, pixel) =>
              (
                acc._1 + pixel.color.red,
                acc._2 + pixel.color.green,
                acc._3 + pixel.color.blue
              )
            )
        )
        .foldLeft((0, 0, 0))((acc, rgb) =>
          (acc._1 + rgb._1, acc._2 + rgb._2, acc._3 + rgb._3)
        )

    def compressForUpperLeft(pixel: Pixel): Pixel =
      val newX = pixel.x / config.samplingFactor
      val newY = pixel.y / config.samplingFactor
      val (redSum, greenSum, blueSum) = sumColorsForUpperLeft(pixel)
      val newColor = Color(
        redSum / config.samplingFactor,
        greenSum / config.samplingFactor,
        blueSum / config.samplingFactor
      )
      pixel.copy(x = newX, y = newY, color = newColor)

    image
      .foldLeft(Vector.empty[Vector[Pixel]])((imageAcc, row) =>
        if (row(0).y % config.samplingFactor == 0)
          imageAcc :+
            row.foldLeft(Vector.empty[Pixel])((rowAcc, pixel) =>
              if (pixel.x % config.samplingFactor == 0)
                rowAcc :+ compressForUpperLeft(pixel)
              else rowAcc
            )
        else imageAcc
      )

  def correctImage(imageRefs: ImageRefs[F]): F[Image] =
    for {
      image <- imageRefs.get
      maxHits = calculateMaxHits(image)
      correctedImage = correctPixels(image, maxHits)
      compressedImage = compressImage(correctedImage)
    } yield compressedImage

}
