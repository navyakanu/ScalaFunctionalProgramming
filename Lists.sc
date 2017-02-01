

object Session {


  def myReduce(xs: List[Int]): Int = {

    if (xs.isEmpty) return 0
    else
      (xs.head) + myReduce(xs.tail)

  }

  myReduce(List(1,4,5,6))




  def myMap(fun:Int=>Int,xs: List[Int]): List[Int] = {


    if (xs.isEmpty) return List.empty
    else
      return (fun(xs.head) :: myMap(fun,xs.tail))

  }

  myMap(square,List(4,5,6,7))


  def square(x: Int): Int ={
    return x*x

  }





  def myfilter(fun: Int=>Boolean, xs: List[Int]): List[Int] = {

        if(xs.isEmpty) return List.empty

        else

          if (fun(xs.head) )
            return (xs.head) :: myfilter(fun,xs.tail)
          else
            return myfilter(fun,xs.tail)
   }

    def findEvenNumbers(x: Int):Boolean =   x%2==0




//
//  val abc = List[Int]
//  abc :: List[Int] = List(1,2)
//
//  List[Int] = List(12,13,14,15,16)



  myfilter(findEvenNumbers,List(12,13,14,15,16))

}
