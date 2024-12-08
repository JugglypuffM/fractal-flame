package domain.image

import cats.Applicative
import cats.effect.{Async, Ref}
import cats.syntax.all.{catsSyntaxApplicativeId, toTraverseOps}

object Image {
  type Image[F[_]] = Vector[Vector[Ref[F, Pixel]]]

  def empty[F[_]: Async](width: Int, height: Int): F[Image[F]] =
    Vector
      .tabulate(height, width)((i, j) => Ref.of[F, Pixel](Pixel(j, i)))
      .traverse(_.sequence)

  implicit class ImageOps[F[_]](val grid: Image[F]) extends AnyVal {
    def updatePixel(x: Int, y: Int, color: Color)(implicit
        F: Applicative[F]
    ): F[Unit] = {
      if ((x >= grid(0).length) || (y >= grid.length) || (x < 0) || (y < 0)) {
        F.pure(())
      } else
        grid(y)(x).update { pixel =>
          if (pixel.hits == 0)
            pixel.copy(color = color, hits = 1)
          else {
            val newRed = (pixel.color.red + color.red) / 2
            val newGreen = (pixel.color.green + color.green) / 2
            val newBlue = (pixel.color.blue + color.blue) / 2
            val newColor = Color(newRed, newGreen, newBlue)
            pixel.copy(color = newColor, hits = pixel.hits + 1)
          }
        }
    }
  }
}
