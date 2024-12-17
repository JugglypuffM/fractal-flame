import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp}
import generation.{ImageCorrector, ImageGenerator, MyRandomImpl}
import io.{ConfigReader, ImageSaver}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      config <- ConfigReader.readConfig[IO](args)
      random <- Random.scalaUtilRandom[IO]
      image <- ImageGenerator[IO](
        config,
        MyRandomImpl[IO](random)
      ).generateImage
      correctedImage <- ImageCorrector[IO](config).correctImage(image)
      _ <- ImageSaver[IO](config).saveImage(correctedImage)
    } yield ExitCode.Success

}
