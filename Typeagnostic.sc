import scala.runtime.Nothing$

object session {


  //Mymap *****************************************************************************************
  def myMap[T](fun: T => T, xs: List[T]): List[T] = {

    if (xs.isEmpty) return List.empty
    else
      return (fun(xs.head) :: myMap(fun, xs.tail))
  }


  myMap(square[String], List("Navya", "NAVYA", "Navya", "Navya"))
  myMap(square[Int], List(1, 2, 3, 4, 6))
  myMap(square[Double], List(1.2, 2.4, 3.6, 4.8, 6.9))


  //MyTake*******************************************************************************************


  def myTake[T](n: Int, list: List[T]): List[T] = {
    if (n == 0)
      return List.empty
    else
      return list.head :: myTake(n - 1, list.tail)

  }


  myTake(1, List("A", "B"))
  myTake(4, List(1, 2, 3, 4, 6))


  //MyFilter****************************************************************************************


  def myfilter[T](fun: T => Boolean, xs: List[T]): List[T] = {

    if (xs.isEmpty) return List.empty
    else if (fun(xs.head))
      return (xs.head) :: myfilter(fun, xs.tail)
    else
      return myfilter(fun, xs.tail)
  }

  def findEvenNumbers[T](x: T): Boolean = 2 % 2 == 0


  myfilter(findEvenNumbers, List(1, 2, 3, 4))
  myfilter(findEvenNumbers, List("A", "B", "L", "D"))

  //*******************************************************************************************************

  //take reverse
  def myTakeReverse[T](n: Int, list: List[T]): List[T] = {

    if (n == list.length)
      return List.empty
    else
      return list.head :: myTakeReverse(n, list.tail)

  }


  myTakeReverse(1, List(1, 2, 3, 4, 6))

  //*******************************************************************************************************


  def myDrop[T](n: Int, list: List[T]): List[T] = {

    if (n == list.length)
      return List.empty
    else
      return list(n) :: myDrop(n, list.tail)

  }


  myDrop(2, List(1, 2, 3, 4, 6))
  myDrop(4, List(1, 2, 3, 4, 6))
  myDrop(2, List("A", "B", "C"))


  //*******************************************************************************************************


  //takewhile

  def takeWhile[T](fun: T => Boolean, n: Int, list: List[T]): List[T] = {
    if (n == 0)
      return List.empty
    else if (fun(list.head))
      return (list.head) :: takeWhile(fun, n - 1, list.tail)
    else
      return takeWhile(fun, n - 1, list.tail)

  }


  takeWhile(findEvenNumbers, 5, List(1, 2, 3, 4, 5))

  takeWhile(findEvenNumbers, 1, List("A", "B", "C"))


  //zip


  //*******************************************************************************************************


  def myZip[T](fun: (T, T) => T, list1: List[T], list2: List[T]): List[T] = {
    if (list1.isEmpty || list2.isEmpty) {
      return List.empty
    }
    else
      return fun(list1.head, list2.head) :: myZip(fun, list1.tail, list2.tail)
  }

  def Add(x1: Int, x2: Int): Int = {
    return x1 + x2
  }


  def multiplyString(x1: String, x2: String): String = {
    return x1 + x2
  }


  myZip(Add, List(1, 2, 3, 4, 6), List(1, 2, 3, 4, 5))
  myZip(multiplyString, List("A", "B", "C", "D", "E"), List("A"))


  //*******************************************************************************************************

  // reduce


  def myReduce[T](fun: T => T, combine: (T, T) => T, list: List[T], zero: T): T = {


    if (list.isEmpty) {
      return zero
    }
    else
      combine(fun(list.head), myReduce(fun, combine, list.tail, zero))

  }

  myReduce(square[Int], addcombine, List(1, 2, 3, 5), 0)


  def addcombine(x: Int, y: Int): Int = {

    return x + y
  }

  //*******************************************************************************************************


  //1. Flatten - >  [[A]] -> [A]

  def flatten[T](fun: T => T, list: List[List[T]]): List[T] = {
    if (list.length == 0)
      return List.empty
    else
      return (list.head) ::: flatten(fun, list.tail)

  }

  flatten[Int](square[Int], List(List(9, 2, 3), List(3, 4, 5)))
  flatten[String](square[String], List(List("ABC", "DEF", "GHI"), List("JKL", "MNO")))


  def square[T](x: T): T = {
    return x
  }

  def stringFunction[String](x: String): String = {
    return x
  }

  //*******************************************************************************************************


  //  1. Flatmap -> (A->[B]) -> [A]->[B]
  //
  //  (X->(X+1),(X+2),(X+3))
  //
  //    [A] -> [1,2,3]
  //
  //  RETURN — [2,3,4,4,5,6,4,5,6]


  def arithMeticSeries(x: Int): List[Int] = {
    return List(x + 2, x + 4, x + 6)

  }

  def geometricSeries(x: Double): List[Double] = {

    return List(x / 2, x / 4, x / 8)
  }


  def flatMap[T](fun: T => List[T], list: List[T]): List[T] = {
    if (list.isEmpty)
      return List.empty
    else
      return fun(list.head) ::: flatMap(fun, list.tail)

  }

  val A=List(1,2,3)
  val B=List(2.0,4.0,8.0)
  flatMap(arithMeticSeries, A)
  flatMap(geometricSeries, B)


  //*******************************************************************************************************


}



