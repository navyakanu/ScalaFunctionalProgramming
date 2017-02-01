object session {

  def factorial(n: Int) : Int ={

    def fact(n:Int) : Int =

      if ( n == 0) 1
      else  (n * fact(n-1))

    fact(n)
  }

  factorial(5)




}