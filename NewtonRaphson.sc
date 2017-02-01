object session {
  1 + 3

  def abs(x: Double) = if (x < 0) -x else x




  def sqrt(x: Double) = {


    def sqrtFilter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtFilter(improve(guess))


    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtFilter(1)
  }

  sqrt(2)





}







