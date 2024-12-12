package io

import cats.effect.Async
import cats.implicits.{toFlatMapOps, toFunctorOps}
import domain.console.Config
import domain.image.Image.Image
import domain.image.{Image, Pixel}
import fs2.Stream
import fs2.io.file.{Files, Path}

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

class ImageSaver[F[_]: Async](config: Config) {
  private def convertPixel(
      pixel: Pixel,
      buffer: BufferedImage
  ): Unit =
    buffer.setRGB(
      pixel.x,
      pixel.y,
      (pixel.color.red << 16) | (pixel.color.green << 8) | pixel.color.blue
    )

  private def bufferToStream(buffer: BufferedImage): Stream[F, Byte] =
    val outputStream = new ByteArrayOutputStream()
    ImageIO.write(buffer, "png", outputStream)
    Stream.emits(outputStream.toByteArray)

  def saveImage(image: Image): F[Unit] =
    val buffer = new BufferedImage(
      config.resultWidth,
      config.resultHeight,
      BufferedImage.TYPE_INT_RGB
    )

    image.map(_.map(convertPixel(_, buffer)))

    for {
      _ <- Files[F].createDirectories(
        config.filePath.parent.getOrElse(Path("."))
      )

      _ <- bufferToStream(buffer)
        .through(Files[F].writeAll(config.filePath))
        .compile
        .drain
    } yield ()
}
