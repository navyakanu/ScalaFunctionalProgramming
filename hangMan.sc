import sun.invoke.empty.Empty

object Session {

  //************************* HangMan(To get a hang of functional programming in scala *******************


  /*   How to run individual function????

  val ExpectedList = List("C","O","N","S","T","A","N","T")
  val DisplayedList = List("C","$","N","$","$","A","N","T")

  HangMan(" ", DisplayedList, ExpectedList, 6)
  findMatch( "O", DisplayedList, ExpectedList) */




  /* Pending Tasks


    1) Pick randomly from the puzzle set and change the question(replace $'s position)
    2) Display neatly work on STDOUT formatting(excluding the name list)
    3) Work on efficiency of CompareEleement*/


  def CompareElement(ElementToFind: String, ElementExpected: String, x: String): String = {
    x match {
      case  ElementExpected => return ElementExpected
      case _  => return ElementToFind
    }
  }

  def findMatch(x: String, actualList: List[String], expectedList: List[String]): List[String] = {
    actualList match  {
      case  Nil =>   return List.empty
      case  _ =>    return CompareElement(actualList.head, expectedList.head, x) :: findMatch( x, actualList.tail, expectedList.tail)
    }
  }

  def HangMan( x: String, actlist: List[String], explist: List[String], count: Int): List[String] = {
    count match {
      case 0 => return actlist
      case _ => `actlist` match {
        case `explist` => return actlist
        case _ => println("Number of attempts remaining  =  ", count, " to solve  = ")
          print(actlist mkString ("    "))
          val input_read = readLine("\n Enter next letter  and press enter \n")
          print("You entered  = ", input_read)
          val fm = findMatch(input_read, actlist, explist)
          fm match {
            case `actlist` => return HangMan(input_read, findMatch(input_read, actlist, explist), explist, count - 1)
            case _ => return HangMan(input_read, findMatch(input_read, actlist, explist), explist, count)
          }
      }
    }
  }


    def Game(result : List[String],displayedList: List[List[String]],expectedList:List[List[String]],n:Int):List[String] = {

      n match {
        case -1 => println("Game Over!!")
                  return result
        case _ => print("\n")
          return Game(HangMan(" ", displayedList(n), expectedList(n), 6), questions, answers, n - 1)
      }
    }







  val answers = List( List("C","O","N","S","T","A","N","T","I","N","O","P","L","E"),
                      List("C","O","N","S","E","R","V","A","T","I","V","E"),
                      List("C","A","M","P","A","I","G","N"),
                      List("C","O","M","B","A","T"),
                      List("C","O","M","E"))
  val questions = List(List("C","$","N","S","$","A","N","$","I","N","$","$","$","E"),
                      List("C","$","N","$","E","R","$","A","$","I","$","E"),
                      List("C","A","$","$","A","I","$","N"),
                      List("$","O","$","B","$","T"),
                      List("C","$","M","E"))




  Game(List(" "),questions,answers,questions.length-1)








}