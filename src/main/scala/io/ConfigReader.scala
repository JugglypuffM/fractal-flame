package io

import cats.Applicative
import cats.implicits.*
import domain.console.Config
import domain.image.Color
import domain.transforms.{Affine, Transform, Variation}
import fs2.io.file.Path
import scopt.OParser

import scala.util.{Random, Try}

object ConfigReader {
  private val builder = OParser.builder[Config]
  private val parser: OParser[Unit, Config] = {
    import builder.*

    val processors = Runtime.getRuntime.availableProcessors()

    def parseAffine(str: String): Affine =
      val parameterArray = str.split(",").map(_.toDouble)

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
      )

    def parseVariation(number: Int): Variation =
      number match
        case 1 => Variation.Sinusoidal
        case 2 => Variation.Spherical
        case 3 => Variation.Swirl
        case 4 => Variation.Horseshoe
        case _ => Variation.Linear

    def validatePositive(int: Int): Either[String, Unit] =
      if (int > 0) success
      else failure("Value should be positive, NO SAD VALUES ARE ALLOWED!!!")

    def validateThreads(int: Int): Either[String, Unit] =
      if ((int > 0) && (int <= processors)) success
      else
        failure(
          s"Invalid thread amount: it should be between 1 and $processors for your PC"
        )

    def validatePath(str: String): Either[String, Unit] =
      Try(Path(str)).map(_ => success).getOrElse(failure("Invalid file path"))

    def validateAffine(str: String): Either[String, Unit] =
      Try(parseAffine(str))
        .map(_ => success)
        .getOrElse(failure("Invalid affine transform format"))

    OParser.sequence(
      programName("fractal-flame"),
      opt[Int]("samples")
        .validate(validatePositive)
        .action((x, c) => c.copy(iterations = x))
        .text("Amount of samples"),
      opt[Int]("iterations")
        .validate(validatePositive)
        .action((x, c) => c.copy(iterations = x))
        .text("Amount of plotting iterations"),
      opt[Int]("threads")
        .validate(validateThreads)
        .action((x, c) => c.copy(threads = x))
        .text("Amount of threads to process the image"),
      opt[Int]("width")
        .validate(validatePositive)
        .action((x, c) => c.copy(width = x))
        .text("Width of resulting image"),
      opt[Int]("height")
        .validate(validatePositive)
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
      opt[Int]("affine-count")
        .validate(validatePositive)
        .action((x, c) => c.copy(affineCount = x))
        .text(
          "Amount of affine transforms which can be applied to point during generation"
        ),
      opt[String]("affine")
        .unbounded()
        .validate(validateAffine)
        .action((x, c) => c.copy(affines = parseAffine(x) :: c.affines))
        .text("Array of the form r,g,b,a,b,c,d,e,f"),
      opt[Int]("variation")
        .required()
        .unbounded()
        .action((x, c) =>
          c.copy(variations = parseVariation(x) :: c.variations)
        )
        .text("""
                |Variation number =
                |1 - Sinusoidal
                |2 - Spherical
                |3 - Swirl
                |4 - Horseshoe
                |other - Linear
                |""".stripMargin),
      opt[Int]("symmetry")
        .validate(validatePositive)
        .action((x, c) => c.copy(symmetry = x))
        .text("Symmetry parameter"),
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
    val config = OParser
      .parse(parser, args, Config())
      .getOrElse(throw RuntimeException("Unexpected error while parsing"))

    config
      .copy(affines =
        config.affines ++ List
          .range(0, config.affineCount - config.affines.length)
          .map(_ => Affine.generateRandomAffine)
      )
      .pure[F]
}
