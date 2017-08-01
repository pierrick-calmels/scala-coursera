import scala.annotation.tailrec

object session {
  def sqrt(x: Double): Double = {
    def abs(x: Double) = if (x < 0) -x else x

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  def factorial(n: Int): Int = {
    @tailrec
    def loop(accumulator: Int, n: Int): Int =
      if (n == 0) accumulator else loop(accumulator*n, n-1)
    loop(1, n)
  }
  factorial(4)

}