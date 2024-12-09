package domain.transforms

case class Point(x: Double, y: Double) {
  def rotate(theta: Double): Point =
    val sint = math.sin(theta)
    val cost = math.cos(theta)
    Point(x * cost - y * sint, x * sint + y * cost)
}
