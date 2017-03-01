object session  {

//abstract class contains members which must be missing in implementation

  abstract class IntSet {

    def inc(x: Int) : IntSet
    def contains(x: Int): Boolean


  }


  class Empty extends IntSet {

    def contains(x: Int) : Boolean = false

    def inc(x: Int) : IntSet = new NonEmpty(x, new Empty, new Empty)

  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {


    def contains(x: Int) : Boolean = {

      if ( x < elem ) left contains x
      else if (x > elem ) right contains x
      else true

    }




    def inc(x: Int) : IntSet = {

      if (x < elem) new NonEmpty(elem, left inc x, right)
      else if (x > elem ) new NonEmpty(elem, left,right inc x)
      else this


    }





  }






}