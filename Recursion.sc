object session {

  def factorial(n: Int) : Int ={

    def fact(n:Int) : Int =

      if ( n == 0) 1
      else  (n * fact(n-1))

    fact(n)
  }

  factorial(5)





  def sum(f: Int => Int,a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1,f(a)+acc )
    }

    loop(a,b)
  }


  def square1(x: Int): Int = {
    return x+x
  }

 sum(square1, 3, 5)




}