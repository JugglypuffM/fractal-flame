package domain.image

case class Pixel(x: Int, y: Int, color: Color = Color(0, 0, 0), hits: Int = 0)
