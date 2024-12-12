import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxApplicativeId
import domain.repository.ImageRepository
import doobie.ExecutionContexts
import doobie.hikari.HikariTransactor
import generation.ImageGenerator
import io.{ConfigReader, ImageSaver}

object Main extends IOApp {
  private val transactor: Resource[IO, HikariTransactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32)
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.postgresql.Driver",
        "jdbc:postgresql://localhost:8080/postgres",
        "postgres",
        "grespost",
        ce
      )
    } yield xa

  override def run(args: List[String]): IO[ExitCode] =
    for {
      xa <- transactor.allocated
      config <- ConfigReader.readConfig[IO](args)
      random <- Random.scalaUtilRandom[IO]
      image <- ImageRepository[IO](xa._1, config).pure[IO]
      _ <- image.initialize()
      _ <- ImageGenerator[IO](image, config, random).generateImage
      maxHits <- image.findMaxHits
      _ <- image.correct(config.gamma, maxHits)
      _ <- ImageSaver[IO](image, config).saveImage
    } yield ExitCode.Success
}
