object Session {

  //************************* HangMan(To get a hang of functional programming in scala *******************
  /* Pending Tasks

    1) Have more puzzle questions
    2) Pick randomly from the puzzle set
    3) Display neatly work on STDOUT formatting(excluding the name list)
    4) Work on efficiency of CompareEleement*/


  // Create a key value pair

  // To select a question, pick 1 randomly




  def CompareElement(ElementToFind: String, ElementExpected: String, x: String): String = {
    if (ElementExpected.equalsIgnoreCase(x))
      return ElementExpected
    else {
      return ElementToFind
    }
  }

  def findMatch(x: String, actualList: List[String], expectedList: List[String]): List[String] = {
    if (actualList.isEmpty)
      return List.empty
    else {
      return CompareElement(actualList.head, expectedList.head, x) :: findMatch( x, actualList.tail, expectedList.tail)
    }
  }

  def HangMan( x: String, actlist: List[String], explist: List[String], count: Int): List[String] = {

    if (count < 1 || !actlist.contains("$")) {
      println("Game Over \n")
      return actlist
    }
    else {
      print("Number of attempts remaining  =  ", count,  " to solve  =  \n")
      print(actlist mkString("    "))
      val input_read = readLine("\n Enter next letter  and press enter \n")
      print ("You entered  = ", input_read)
      val fm = findMatch(input_read, actlist, explist)
       if (fm.equals(actlist))
           return HangMan(input_read, findMatch(input_read, actlist, explist), explist, count-1)
       else
         return HangMan(input_read, findMatch(input_read, actlist, explist), explist, count)

    }
  }

  val ExpectedList = List("C","O","N","S","T","A","N","T")
  val DisplayedList = List("C","$","N","$","$","A","N","T")



  HangMan(" ", DisplayedList, ExpectedList, 6)


  //val ExpectedList = List("A", "B", "C")
  //val DisplayedList = List("$", "B", "$")

  //  val single_input_value = "O"
  findMatch( "O", DisplayedList, ExpectedList)


}