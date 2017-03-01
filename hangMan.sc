object Session  {

//************************* HangMan(To get a hang of functional programming in scala *******************
/* Pending Tasks

  1) Read input from console till the puzzle is solved and the winner is declared
  2) Have more puzzle questions
  3) Pick randomly from the puzzle set
  4) End the game if the count exceeds 6 and display "Hanged" message to user
  5) Do not increase count on positive attempts
  6) Display neatly (excluding the name list)*/


  // Create a key value pair
  val ExpectedList = List("C","O","N","T","R","Y")
  val DisplayedList = List("C","$","N","$","R","$")

  // To select a question, pick 1 randomly






  val input_value = List("O","R","6","7","8","9","8","9","8","7")



  HangMan[String](CompareElement,input_value,DisplayedList,ExpectedList,0)




//  val single_input_value = "O"
//  findMatch[String](CompareElement,single_input_value,DisplayedList,ExpectedList)


  def HangMan[T](fun: (T,T,T) => T, x: List[T], actlist: List[T],explist: List[T],count: Int): List[T] = {

    if(x.isEmpty || count > 6)
      return actlist
    else {
      val fm = findMatch[T](CompareElement,x.head,actlist,explist)
      return HangMan[T](CompareElement,x.tail,fm,explist,count+1)
    }
  }


  def findMatch[T](fun: (T,T,T) => T, x: T, actlist: List[T],explist: List[T]): List[T] = {
    if (actlist.isEmpty)
      return List.empty
    else {
      return fun(actlist.head,explist.head, x) :: findMatch(fun, x, actlist.tail,explist.tail)
     }
  }

  def CompareElement[T](listElementToFind: T,listElementToPresent:T,x: T): T = {
    if (listElementToPresent.equals(x))
        return listElementToPresent
     else {
       return listElementToFind
     }
   }






















}