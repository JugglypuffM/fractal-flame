package domain.console

import domain.image.Color
import domain.transforms.{Affine, Transform, Variation}
import fs2.io.file.Path

case class Config(
    iterations: Int = 1000000,
    threads: Int = 1,
    width: Int = 1920,
    height: Int = 1080,
    xMin: Double = -1.777,
    xMax: Double = 1.777,
    yMin: Double = -1,
    yMax: Double = 1,
    gamma: Double = 2.2,
    transforms: List[Transform] = List.empty,
    filePath: Path = Path("./image.png")
)
