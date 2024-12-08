import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp}
import generation.{ImageCorrector, ImageGenerator}
import io.{ConfigReader, ImageSaver}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      config <- ConfigReader.readConfig[IO](args)
      random <- Random.scalaUtilRandom[IO]
      image <- ImageGenerator[IO](config, random).generateImage
      _ <- ImageSaver[IO](config).saveImage(image)
    } yield ExitCode.Success

}
