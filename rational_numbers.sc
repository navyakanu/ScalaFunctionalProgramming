
object Session {


  val x = new Rational(1,3)
  x.numer
  x.denom

  val y = new Rational(5,7)
  y.add(x)
  y.neg

  y.add(y)

  val z = new Rational(3,2)

  x.sub(y).sub(z)
  y.less(z)
  y.max(z)

  class Rational(x: Int, y: Int) {

    require(y != 0, "Denominator must be non zero")

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)


    def this(x: Int) = this(x, 1)

    def numer = x ;

    def denom = y ;

    def add(the: Rational) = new Rational(numer * the.denom + the.numer * denom, denom * the.denom)

    def neg() = new Rational(-numer, denom)

    def sub(the: Rational) = {
      add(the.neg)
    }

    def less(the: Rational) = numer * the.denom < the.numer * denom

    def max(the: Rational) = if (this.less(the)) false else true

    override def toString = {
      val g = gcd(x,y)

      numer/g + "/" + denom/g
    }
  }





}


