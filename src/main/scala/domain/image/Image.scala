package domain.image

case class Image(grid: Vector[Vector[Pixel]]) {
  def updatePixel(x: Int, y: Int, color: Color): Image = {
    if ((x >= grid(0).length) || (y >= grid.length) || (x < 0) || (y < 0))
      Image(grid)
    else if (grid(y)(x).hits == 0)
      val pixel = grid(y)(x)
      Image(grid.updated(y, grid(y).updated(x, pixel.copy(color = color, hits = 1))))
    else
      val pixel = grid(y)(x)
      val newRed = (pixel.color.red + color.red) / 2
      val newGreen = (pixel.color.green + color.green) / 2
      val newBlue = (pixel.color.blue + color.blue) / 2
      val newColor = Color(newRed, newGreen, newBlue)
      Image(
        grid.updated(y, grid(y).updated(x, pixel.copy(color = newColor, hits = pixel.hits + 1)))
      )
  }
}

object Image {
  def empty(width: Int, height: Int): Image = Image(
    Vector.tabulate(height, width)((i, j) => Pixel(j, i))
  )
}
