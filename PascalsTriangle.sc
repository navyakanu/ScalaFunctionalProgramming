// Pascals traingle


object PascalsT {


      def pascal(c:Int, r:Int):Int =
        if ( c == 0 ||  c == r)
          return 1
      else
          return pascal(c,r-1)+pascal(c-1,r-1)


  pascal(21,21)


}


// Parenthisis matching

object ParenthisisMatching {

  def balance(chars: List[Char]): Boolean = {

    def status(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) {
        open == 0
      }
      else {
        chars.head match {
          case '(' => status(chars.tail, open + 1)
          case ')' => open > 0 && status(chars.tail, open - 1)
          case _ =>   status(chars.tail, open)
        }
      }
    }
    status(chars, 0)
  }


  balance(List('(', ')', ')', '('))
  balance(List('(', ')', '(', ')'))
  balance(List('(','H',')'))



  def balanced(chars: List[Char], open: Int): Boolean = {
    if (chars.isEmpty) open == 0

    else if (chars.head == '(') balanced(chars.tail, open + 1)
    else if (chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
    else balanced(chars.tail, open)
  }

}


///


object CountChange {

  def countChange(money: Int, coins: List[Int]): Int = {
    def countTheWays(money:Int, coins: List[Int],count:Int):Int = {
      if ( money == 0 ) {
        return count
      }
       if(money % coins.head == 0 ) {
         countTheWays(money, coins.tail, count+1)
       }
       else
         countTheWays(money- coins.head ,coins.tail,count+1)

    }
    countTheWays(money,coins,0)
  }
}


def countChange(money: Int, coins: List[Int]): Int = {
  def change(m: Int, coinList: List[Int], count: Int): Int =
    m match {
      case _ if m < 0 => count
      case _ if coinList.isEmpty => { m match {
                                         case 0 => count + 1
                                         case _ => count
                                              }
                                    }
      case _ => change(m, coinList.tail, count) + change(m - coinList.head, coinList, count)
    }
  change(money, coins, 0)
}