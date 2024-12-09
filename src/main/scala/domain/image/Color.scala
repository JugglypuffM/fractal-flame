package domain.image

case class Color(red: Int, green: Int, blue: Int) {
  def correct(hits: Int, maxHits: Int, gamma: Double): Color =
    def correctInt(value: Int): Int =
      val logScaled = math.log10(hits) / math.log10(maxHits)
      (math.pow(logScaled, 1.0 / gamma)*value).toInt

    Color(
      correctInt(red),
      correctInt(green),
      correctInt(blue)
    )
}
