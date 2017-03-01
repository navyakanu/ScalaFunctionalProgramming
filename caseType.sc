object session1 {



//Definition of class constructor

class MyBox[T] (val Value: T) {

}



  def square[T](x: T): T = {
    return x
  }

 def mapMyBox[T,A](fun: T => A, mb: MyBox[T] ): MyBox[A] ={

      new MyBox(fun(mb.Value))

 }



mapMyBox[Int,Int](square(1),new MyBox(12))

 def joinMyBox[T](mb1: MyBox[MyBox[T]]): T ={


        return mb1.Value.Value
 }


  joinMyBox[Int](new MyBox(new MyBox(15)))






  def flatMap[T,A](fun: T=> MyBox[A],mbA: MyBox[A]): MyBox[T] ={


  }




def ListFunc(listIn: List[Int]) : List[Int] = {
  return List(1,2,3,4)
}


def StringFun(stringValue : String) :String ={
  return "A"
}



}