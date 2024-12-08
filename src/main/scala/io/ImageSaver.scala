package io

import cats.effect.{Async, Ref}
import cats.implicits.{toFlatMapOps, toFunctorOps}
import domain.console.Config
import domain.image.{Image, Pixel}
import fs2.Stream
import fs2.io.file.{Files, Path}

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

class ImageSaver[F[_]: Async](config: Config) {
  private def convertPixel(
      pixel: Pixel,
      ref: Ref[F, BufferedImage]
  ): F[Unit] = {
    val rgb =
      (pixel.color.red << 16) | (pixel.color.green << 8) | pixel.color.blue
    ref.update(buffer => {
      buffer.setRGB(pixel.x, pixel.y, rgb)
      buffer
    })
  }

  private def bufferToStream(buffer: BufferedImage): Stream[F, Byte] =
    val outputStream = new ByteArrayOutputStream()
    ImageIO.write(buffer, "png", outputStream)
    Stream.emits(outputStream.toByteArray)

  def saveImage(image: Image): F[Unit] =
    for {
      bufferRef <- Ref.of[F, BufferedImage](
        new BufferedImage(
          config.width,
          config.height,
          BufferedImage.TYPE_INT_RGB
        )
      )

      _ <- Stream
        .emits(image.grid)
        .flatMap(Stream.emits(_))
        .parEvalMapUnordered(config.threads)(pixel =>
          convertPixel(pixel, bufferRef)
        )
        .compile
        .drain

      buffer <- bufferRef.get

      _ <- Files[F].createDirectories(
        config.filePath.parent.getOrElse(Path("."))
      )

      _ <- bufferToStream(buffer)
        .through(Files[F].writeAll(config.filePath))
        .compile
        .drain
    } yield ()
}
