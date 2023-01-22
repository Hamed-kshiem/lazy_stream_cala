package stream

object Sqrt {

  def sqrtStream(x: Double): LStream[Double] = ???

  def approximate[T](pred: T => Boolean, sequ: LStream[T]): Option[T] = ???

  def approximateSqrt(x: Double, epsilon: Double): Option[Double] = ???

  def main(args: Array[String]): Unit = {

    val sqrt9 = approximateSqrt(9.0, 0.000000001)
    println(s"Square root of 9 = $sqrt9")

    val sqrt16 = approximateSqrt(16.0, 0.000000001)
    println(s"Square root of 16 = $sqrt16")

    val sqrt25 = approximateSqrt(25.0, 0.000000001)
    println(s"Square root of 25 = $sqrt25")

  }
}
