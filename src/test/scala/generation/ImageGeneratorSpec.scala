package generation

import cats.effect.*
import cats.effect.std.Random
import cats.effect.testing.scalatest.AsyncIOSpec
import domain.console.Config
import domain.image.Image.ImageOps
import domain.image.{Color, Image, Pixel}
import domain.transforms.Variation.{Linear, Sinusoidal}
import domain.transforms.{Affine, Point, Variation}
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class ImageGeneratorSpec
    extends AsyncWordSpec
    with AsyncIOSpec
    with Matchers
    with AsyncMockFactory {

  "ImageGenerator" should {

    val config = Config(
      samples = 2,
      iterations = 5,
      xMin = -1,
      xMax = 1,
      renderWidth = 10,
      renderHeight = 10,
      resultWidth = 5,
      resultHeight = 5,
      symmetry = 3,
      samplingFactor = 2,
      affines = List(
        Affine(Color(255, 0, 0), 1, 0, 0, 0, 1, 0),
        Affine(Color(0, 255, 0), 0, 0.5, 0, 0.5, 0, 0)
      ),
      variations = List(Linear, Sinusoidal)
    )

    val mockRandom = mock[MyRandom[IO]]
    val imageGenerator = new ImageGenerator[IO](config, mockRandom)

    "generate random bounded points" in {
      (mockRandom.betweenDouble _)
        .expects(config.xMin, config.xMax)
        .returns(IO.pure(0.5))
        .once()

      (mockRandom.betweenDouble _)
        .expects(config.yMin, config.yMax)
        .returns(IO.pure(-0.5))
        .once()

      val pointIO = imageGenerator.randomBoundedPoint
      pointIO.asserting { point =>
        point shouldBe Point(0.5, -0.5)
      }
    }

    "project point correctly to pixel coordinates" in {
      val point = Point(0.0, 0.0)

      val pixelIO = imageGenerator.projectPoint(point)
      pixelIO.asserting { pixel =>
        pixel.x shouldBe 4
        pixel.y shouldBe 4
      }
    }

    "process rotations correctly" in {
      val point = Point(0.0, 0.0)
      val color = Color(255, 0, 0)

      val processIO = for {
        imageRefs <- Image.empty[IO](config.renderWidth, config.renderHeight)
        _ <- imageGenerator.processRotation(
          Point(0.5, 0.5),
          Color(1, 12, 123),
          imageRefs
        )
        image <- imageRefs.get
      } yield image

      val emptyImage = Vector.tabulate(10, 10)((j, i) => Pixel(i, j))
      val expectedImage = emptyImage
        .updated(
          1,
          emptyImage(1).updated(
            5,
            emptyImage(1)(5).copy(color = Color(1, 12, 123), hits = 1)
          )
        )
        .updated(
          5,
          emptyImage(5).updated(
            1,
            emptyImage(5)(1).copy(color = Color(1, 12, 123), hits = 1)
          )
        )
        .updated(
          7,
          emptyImage(7).updated(
            7,
            emptyImage(7)(7).copy(color = Color(1, 12, 123), hits = 1)
          )
        )

      processIO.asserting(image => image shouldBe expectedImage)
    }

    "process iterations for given points" in {
      (mockRandom.elementOf[Affine] _)
        .expects(config.affines)
        .returns(IO.pure(config.affines.head))
        .once()

      (mockRandom.elementOf[Variation] _)
        .expects(config.variations)
        .returns(IO.pure(config.variations.head))
        .once()

      (mockRandom.elementOf[Affine] _)
        .expects(config.affines)
        .returns(IO.pure(config.affines(1)))
        .once()

      (mockRandom.elementOf[Variation] _)
        .expects(config.variations)
        .returns(IO.pure(config.variations(1)))
        .once()

      (mockRandom.elementOf[Affine] _)
        .expects(config.affines)
        .returns(IO.pure(config.affines.head))
        .once()

      (mockRandom.elementOf[Variation] _)
        .expects(config.variations)
        .returns(IO.pure(config.variations(1)))
        .once()

      (mockRandom.elementOf[Affine] _)
        .expects(config.affines)
        .returns(IO.pure(config.affines.head))
        .once()

      (mockRandom.elementOf[Variation] _)
        .expects(config.variations)
        .returns(IO.pure(config.variations(1)))
        .once()

      (mockRandom.elementOf[Affine] _)
        .expects(config.affines)
        .returns(IO.pure(config.affines(1)))
        .once()

      (mockRandom.elementOf[Variation] _)
        .expects(config.variations)
        .returns(IO.pure(config.variations.head))
        .once()

      val processIO = for {
        imageRefs <- Image.empty[IO](config.renderWidth, config.renderHeight)
        _ <- imageGenerator.processIteration(Point(1, 0.5), imageRefs)
        image <- imageRefs.get
      } yield image

      val emptyImage = Vector.tabulate(10, 10)((j, i) => Pixel(i, j))
      val expectedImage = emptyImage
        .updated(
          2,
          emptyImage(2).updated(
            6,
            emptyImage(2)(6).copy(color = Color(191, 63, 0), hits = 3)
          )
        )
        .updated(
          3,
          emptyImage(3).updated(
            4,
            emptyImage(3)(4).copy(color = Color(0, 255, 0), hits = 1)
          )
        )
        .updated(
          4,
          emptyImage(4).updated(
            2,
            emptyImage(4)(2).copy(color = Color(191, 63, 0), hits = 3)
          )
        )
        .updated(
          5,
          emptyImage(5)
            .updated(
              3,
              emptyImage(5)(3).copy(color = Color(0, 255, 0), hits = 1)
            )
            .updated(
              6,
              emptyImage(5)(6).copy(color = Color(0, 255, 0), hits = 1)
            )
        )
        .updated(
          7,
          emptyImage(7)
            .updated(
              6,
              emptyImage(7)(6).copy(color = Color(191, 63, 0), hits = 3)
            )
            .updated(
              9,
              emptyImage(7)(9).copy(color = Color(255, 0, 0), hits = 1)
            )
        )
        .updated(
          8,
          emptyImage(8).updated(
            0,
            emptyImage(8)(0).copy(color = Color(255, 0, 0), hits = 1)
          )
        )

      processIO.asserting(image => image shouldBe expectedImage)
    }

    "generate faster in multi-thread" in {
      def measureTime(generator: ImageGenerator[IO]): IO[Long] =
        for {
          start <- IO(System.nanoTime())
          _ <- generator.generateImage
          end <- IO(System.nanoTime())
        } yield end - start

      val baseConfig = Config(
        samplingFactor = 2,
        symmetry = 3,
        affines = List(
          Affine(Color(255, 0, 0), 1, 0, 0, 0, 1, 0),
          Affine(Color(0, 255, 0), 0, 0.5, 0, 0.5, 0, 0),
          Affine(Color(0, 0, 255), 0, 0, 0.5, 0, 0, 0.5)
        ),
        variations = List(Linear, Sinusoidal)
      )
      val processors = Runtime.getRuntime.availableProcessors()
      val halfMaxThreadsConfig = baseConfig.copy(threads = processors / 2)
      val maxThreadsConfig = baseConfig.copy(threads = processors)

      for {
        random <- Random.scalaUtilRandom[IO]
        singleThreadedGenerator = ImageGenerator[IO](
          baseConfig,
          MyRandomImpl[IO](random)
        )
        halfMaxThreadedGenerator = ImageGenerator[IO](
          halfMaxThreadsConfig,
          MyRandomImpl[IO](random)
        )
        maxThreadedGenerator = ImageGenerator[IO](
          maxThreadsConfig,
          MyRandomImpl[IO](random)
        )
        singleThreadTime <- measureTime(singleThreadedGenerator)
        halfMaxThreadTime <- measureTime(halfMaxThreadedGenerator)
        maxThreadTime <- measureTime(maxThreadedGenerator)
      } yield {
        println(s"Single-threaded time: ${singleThreadTime / 1e6} ms")
        println(
          s"${processors / 2}-threaded time: ${halfMaxThreadTime / 1e6} ms"
        )
        println(s"$processors-threaded time: ${maxThreadTime / 1e6} ms")

        halfMaxThreadTime should be < singleThreadTime
        maxThreadTime should be <= halfMaxThreadTime - (5 * halfMaxThreadTime) / 100
      }

    }
  }
}
