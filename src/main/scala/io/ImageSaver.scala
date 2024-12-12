package io

import cats.effect.Async
import cats.effect.implicits.concurrentParTraverseOps
import cats.implicits.{toFlatMapOps, toFunctorOps}
import domain.console.Config
import domain.repository.ImageRepository
import fs2.Stream
import fs2.io.file.{Files, Path}

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

class ImageSaver[F[_]: Async](image: ImageRepository[F], config: Config) {
  def createBufferedImage(buffer: BufferedImage, pageSize: Int = 1000): F[BufferedImage] = {
    def processPage(offset: Int): F[Unit] = {
      for {
        pixels <- image.getPixelsPage(offset, pageSize)
        _ <- pixels.parTraverseN(config.threads)(pixel =>
          Async[F].pure({
            buffer.setRGB(
              pixel.x,
              pixel.y,
              (pixel.color.red << 16) | (pixel.color.green << 8) | pixel.color.blue
            )
          })
        )
        _ <-
          if (pixels.nonEmpty) processPage(offset + pageSize) else Async[F].unit
      } yield ()
    }

    for{
      _ <- processPage(0)
    } yield buffer
  }

  private def bufferToStream(buffer: BufferedImage): Stream[F, Byte] =
    val outputStream = new ByteArrayOutputStream()
    ImageIO.write(buffer, "png", outputStream)
    Stream.emits(outputStream.toByteArray)

  def saveImage: F[Unit] =
    val buffer = new BufferedImage(
      config.width,
      config.height,
      BufferedImage.TYPE_INT_RGB
    )

    for {
      buffer <- createBufferedImage(buffer, 1000000)

      _ <- Files[F].createDirectories(
        config.filePath.parent.getOrElse(Path("."))
      )

      _ <- bufferToStream(buffer)
        .through(Files[F].writeAll(config.filePath))
        .compile
        .drain
    } yield ()
}
