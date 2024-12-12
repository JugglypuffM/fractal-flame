package domain.repository

import cats.effect.kernel.Async
import domain.console.Config
import domain.image.{Color, Pixel}
import doobie.implicits.{toConnectionIOOps, toSqlInterpolator}
import doobie.util.transactor.Transactor
import cats.implicits.toFunctorOps
import cats.implicits.toFlatMapOps

class ImageRepository[F[_]: Async](xa: Transactor[F], config: Config) {
  def initialize(): F[Unit] =
    sql"""
        DROP TABLE IF EXISTS pixels;
        CREATE TABLE IF NOT EXISTS pixels (
            x int,
            y int,
            r int,
            g int,
            b int,
            h int,
            constraint unique_coords unique (x, y)
        )
       """.update.run.map(_ => ()).transact(xa)

  def updatePixel(x: Int, y: Int, color: Color): F[Unit] =
    if ((0 > x) || (config.width <= x) || (0 > y) || (config.height <= y))
      Async[F].pure(())
    else
      sql"""
      INSERT INTO pixels (x, y, r, g, b, h)
      VALUES ($x, $y, ${color.red}, ${color.green}, ${color.blue}, 1)
      ON CONFLICT ON CONSTRAINT unique_coords DO UPDATE
      SET r = (pixels.r + ${color.red}) / 2,
          g = (pixels.g + ${color.green}) / 2,
          b = (pixels.b + ${color.blue}) / 2,
          h = pixels.h + 1
    """.update.run.map(_ => ()).transact(xa)

  def findMaxHits: F[Int] = {
    sql"SELECT MAX(h) FROM pixels".query[Option[Int]].unique.map(_.getOrElse(1)).transact(xa)
  }

  def correct(gamma: Double, maxHits: Int): F[Unit] =
    sql"""
        UPDATE pixels
        SET r = r * POWER(LOG10(h) / LOG10($maxHits), 1 / $gamma),
            g = g * POWER(LOG10(h) / LOG10($maxHits), 1 / $gamma),
            b = b * POWER(LOG10(h) / LOG10($maxHits), 1 / $gamma)
      """.update.run.map(_ => ()).transact(xa)

  def getPixelsPage(offset: Int, limit: Int): F[List[Pixel]] =
    sql"""
        SELECT x, y, r, g, b, h FROM pixels
        ORDER BY x, y
        LIMIT $limit OFFSET $offset
      """.query[Pixel].to[List].transact(xa)
}
