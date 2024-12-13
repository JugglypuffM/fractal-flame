package domain.console

import domain.transforms.{Affine, Variation}
import fs2.io.file.Path

case class Config(
    samples: Int = 10,
    iterations: Int = 1000000,
    threads: Int = 1,
    renderWidth: Int = 1920,
    renderHeight: Int = 1080,
    samplingFactor: Int = 1,
    resultWidth: Int = 1920,
    resultHeight: Int = 1080,
    xMin: Double = -1.777,
    xMax: Double = 1.777,
    yMin: Double = -1,
    yMax: Double = 1,
    gamma: Double = 2.2,
    affineCount: Int = 10,
    variationCount: Int = 3,
    affines: List[Affine] = List.empty,
    variations: List[Variation] = List.empty,
    symmetry: Int = 1,
    filePath: Path = Path("./image.png")
)
