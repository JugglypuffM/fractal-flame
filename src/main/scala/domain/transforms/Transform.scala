package domain.transforms

import cats.Applicative
import cats.implicits.catsSyntaxApplicativeId

case class Transform(affine: Affine, variation: Variation) {
  def apply[F[_]: Applicative](point: Point): F[Point] = variation(
    affine(point)
  ).pure[F]
}
