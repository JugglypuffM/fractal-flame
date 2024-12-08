package domain.image

case class Color(red: Int, green: Int, blue: Int) {
  def correct(gamma: Double): Color =
    def correctInt(value: Int): Int =
      val logScaled = math.log(1 + value) / math.log(256)
      (math.pow(logScaled, 1.0 / gamma)*256).toInt

    Color(
      correctInt(red),
      correctInt(green),
      correctInt(blue)
    )
}
