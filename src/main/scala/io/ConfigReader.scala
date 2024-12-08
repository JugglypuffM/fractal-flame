package io

import cats.Applicative
import cats.implicits.*
import domain.console.Config
import domain.image.Color
import domain.transforms.{Affine, Transform, Variation}
import fs2.io.file.Path
import scopt.OParser

import scala.util.Try

object ConfigReader {
  private val builder = OParser.builder[Config]
  private val parser: OParser[Unit, Config] = {
    import builder.*

    val processors = Runtime.getRuntime.availableProcessors()

    def parseTransform(str: String): Transform =
      val parameterArray = str.split(",").map(_.toDouble)
      val variation = parameterArray(9).toInt match
        case 1 => Variation.Sinusoidal
        case 2 => Variation.Spherical
        case 3 => Variation.Swirl
        case 4 => Variation.Horseshoe
        case _ => Variation.Linear

      Transform(
        Affine(
          Color(
            parameterArray(0).toInt,
            parameterArray(1).toInt,
            parameterArray(2).toInt
          ),
          parameterArray(3),
          parameterArray(4),
          parameterArray(5),
          parameterArray(6),
          parameterArray(7),
          parameterArray(8)
        ),
        variation
      )

    def validateThreads(int: Int): Either[String, Unit] =
      if ((int > 0) && (int <= processors)) success
      else
        failure(
          s"Invalid thread amount: it should be between 1 and $processors for your PC"
        )

    def validatePath(str: String): Either[String, Unit] =
      Try(Path(str)).map(_ => success).getOrElse(failure("Invalid file path"))

    def validateTransform(str: String): Either[String, Unit] =
      Try(parseTransform(str))
        .map(_ => success)
        .getOrElse(failure("Invalid transform format"))

    OParser.sequence(
      programName("fractal-flame"),
      opt[Int]("samples")
        .action((x, c) => c.copy(iterations = x))
        .text("Amount of samples"),
      opt[Int]("iterations")
        .action((x, c) => c.copy(iterations = x))
        .text("Amount of plotting iterations"),
      opt[Int]("threads")
        .validate(validateThreads)
        .action((x, c) => c.copy(threads = x))
        .text(""),
      opt[Int]("width")
        .action((x, c) => c.copy(width = x))
        .text("Width of resulting image"),
      opt[Int]("height")
        .action((x, c) => c.copy(height = x))
        .text("Height of resulting image"),
      opt[Double]("xMin")
        .action((x, c) => c.copy(xMin = x))
        .text(
          "Lower boundary for x component of start point"
        ),
      opt[Double]("xMax")
        .action((x, c) => c.copy(xMax = x))
        .text(
          "Upper boundary for x component of start point"
        ),
      opt[Double]("yMin")
        .action((x, c) => c.copy(yMin = x))
        .text(
          "Lower boundary for y component of start point"
        ),
      opt[Double]("yMax")
        .action((x, c) => c.copy(yMax = x))
        .text(
          "Upper boundary for y component of start point"
        ),
      opt[Double]("gamma")
        .action((x, c) => c.copy(gamma = x))
        .text(
          "Parameter for log-gamma correction"
        ),
      opt[String]("transform")
        .required()
        .unbounded()
        .action((x, c) =>
          c.copy(transforms = parseTransform(x) :: c.transforms)
        )
        .text("""
            |Array of the form r,g,b,a,b,c,d,e,f,variationNumber, where variation number =
            |1 - Sinusoidal
            |2 - Spherical
            |3 - Swirl
            |4 - Horseshoe
            |other - Linear
            |""".stripMargin),
      opt[String]("file")
        .validate(validatePath)
        .action((x, c) => c.copy(filePath = Path(x)))
        .text("Path of resulting image"),
      checkConfig(c =>
        if (c.xMin > c.xMax) failure("Invalid boundary for X")
        else if (c.yMin > c.yMax) failure("Invalid boundary for Y")
        else success
      )
    )
  }

  def readConfig[F[_]: Applicative](args: List[String]): F[Config] =
    OParser
      .parse(parser, args, Config())
      .getOrElse(throw RuntimeException("Unexpected error while parsing"))
      .pure[F]
}
