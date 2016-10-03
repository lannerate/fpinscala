package fpinscala

import org.scalatest.{FunSpec}
import fp.datastruct._
/**
  * Created by apple on 10/1/16.
  */
class ListTest extends FunSpec{

  describe("List Test cases") {

    it("The sum of List(1,2,3,4,5) should be equal 15"){
      assert( List.sum( List(1,2,3,4,5) ) == 15 )
    }


    it("list(1,2,3,4,5) by matching should return 3"){
      val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x,Cons(y,Cons(3,Cons(4, _)))) => x + y
        case Cons(h,t) => h + List.sum(t)
        case _ => 101
      }
      assert(x == 3)
    }

    it("test tail implementation when List.trail(List(1,2,3,4)) should return Cons(2,Cons(3,Cons(4,Nil))"){
      val x = List.tail(List(1,2,3,4));
      val expected = Cons(2,Cons(3,Cons(4,Nil)))

      assert( x == expected )
    }

    it("List.drop(List(1,2,3,4,5,6),2) should return Cons(3,Cons(4,Cons(5,Cons(6,Nil))))"){
      val x = List.drop(List(1,2,3,4,5,6),2)
      val expected = Cons(3,Cons(4,Cons(5,Cons(6,Nil))))

      assert( x == expected )
    }
  }

}
