package generation

import cats.MonadThrow
import cats.effect.std.Random

trait MyRandom[F[_]] {
  def elementOf[A](list: List[A]): F[A]

  def betweenDouble(min: Double, max: Double): F[Double]
}

case class MyRandomImpl[F[_]: MonadThrow](random: Random[F])
    extends MyRandom[F] {
  override def elementOf[A](list: List[A]): F[A] = random.elementOf(list)
  override def betweenDouble(min: Double, max: Double): F[Double] =
    random.betweenDouble(min, max)
}
