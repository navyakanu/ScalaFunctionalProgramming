object Session {





      def pascal(c:Int, r:Int):Int =
        if ( c == 0 ||  c == r)
          return 1
      else
          return pascal(c,r-1)+pascal(c-1,r-1)





  pascal(21,21)


}