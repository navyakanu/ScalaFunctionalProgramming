object session {


//Mymap
  def myMap[T](fun: T => T, xs: List[T]): List[T] = {

    if (xs.isEmpty) return List.empty
    else
      return (fun(xs.head) :: myMap(fun, xs.tail))
  }

  def square[T](x: T): T = {
    return x }

  myMap(square[String],List("Navya","NAVYA","Navya","Navya"))
  myMap(square[Int],List(1, 2, 3, 4, 6))
  myMap(square[Double],List(1.2, 2.4, 3.6, 4.8, 6.9))


  //MyTake


  def myTake[T](n: Int, list: List[T]): List[T] = {
     if (n ==0 )
        return List.empty
      else
          return list.head :: myTake(n-1,list.tail)

      }


  myTake(1,List("A","B"))
  myTake(4, List(1, 2, 3, 4, 6))


  //MyFilter


  def myfilter[T](fun: T=>Boolean, xs: List[T]): List[T] = {

    if(xs.isEmpty) return List.empty
    else

    if ( fun(xs.head) )
      return (xs.head) :: myfilter(fun,xs.tail)
    else
      return myfilter(fun,xs.tail)
  }

  def findEvenNumbers[T](x: T):Boolean = 2%2==0


  myfilter(findEvenNumbers,List(1,2,3,4))
  myfilter(findEvenNumbers,List("A","B","L","D"))



  //take reverse
  def myTakeReverse[T](n: Int, list: List[T]): List[T] = {

    if ( n==list.length )
      return List.empty
    else
      return  list.head:: myTakeReverse(n,list.tail)

  }


  myTakeReverse(1, List(1, 2, 3, 4, 6))


  def myDrop[T](n: Int, list: List[T]): List[T] = {

    if ( n==list.length )
      return List.empty
    else
      return   list(n)::myDrop(n,list.tail)

  }


  myDrop(2, List(1, 2, 3, 4, 6))
  myDrop(4, List(1, 2, 3, 4, 6))
  myDrop(2,List("A","B","C"))





  //takewhile

  def takeWhile[T](fun: T=>Boolean,n:Int,list:List[T]):List[T] ={
    if (n ==0 )
      return List.empty
    else
       if ( fun(list.head) )
      return (list.head) :: takeWhile(fun,n-1,list.tail)
    else
      return takeWhile(fun,n-1,list.tail)

  }



takeWhile(findEvenNumbers,5,List(1,2,3,4,5))

takeWhile(findEvenNumbers,1,List("A","B","C"))


  //zip

  def myZip[T](fun: T => T, list1: List[T],list2: List[T]): List[T] = {


    if (list1.isEmpty || list2.isEmpty){
      return List.empty
    }
    else
      return fun(list1.head,list2.head) :: myZip(fun,list1.tail,list2.tail)

  }

  def multiply[T](x1: T,x2:T): T ={
    return x1
  }

  //reduce
}



