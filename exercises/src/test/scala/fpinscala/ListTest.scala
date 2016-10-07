package fpinscala

import org.scalatest.{FunSpec}
import fp.datastruct._
/**
  * Created by apple on 10/1/16.
  */
class ListTest extends FunSpec{

  describe("Unit Test cases for List") {

    it("The sum of List(1,2,3,4,5) should be equal 15"){
      assert( List.sum( List(1,2,3,4,5) ) == 15 )
    }

    it("the sum of List(10,20,30) should be equal 60"){
      assert(List.sum2(List(10,20,30)) == 60)
    }

    it("The product of List(2,3,4) should return 24"){
      assert(List.product2(List(2,3,4)) == 24)
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

    it("List.dropWhile(List(1,2,3,4,5))(i => i <= 3 ) should return Cons(4,Cons(5,Nil))"){
      val x = List.dropWhile( List(1,2,3,4,5) )(i => i<=3)
      val expected = Cons(4,Cons(5,Nil))

      assert(x == expected)
    }

    it("put the number 1 into the header of list(2,3,4,5) should return Cons(1,Cons(3,Cons(4,Cons(5,Nil))))))"){
      val x = List.setHead(List(2,3,4,5))(1)
      val expected = Cons(1,Cons(3,Cons(4,Cons(5,Nil))))

      assert(x == expected)
    }

    it("List(1,2) append List(3,4) should return List(1,2,3,4)"){
      val x = List.append(List(1,2),List(3,4))
      val expected = Cons(1,Cons(2,Cons(3,Cons(4,Nil))))

      assert( x == expected )
    }

    it("List(1,2) append Nil should return List(1,2)"){
      val x = List.append(List(1,2),Nil)
      val expected = Cons(1,Cons(2,Nil))

      assert(x==expected)
    }

    it("test init() List(1,2,3,4) should return List(1,2,3)"){
      val x = List.init(List(1,2,3,4))
      val expected = Cons(1,Cons(2,Cons(3,Nil)))

      assert(x==expected)
    }

    it("what happens when foldRight(List(1,2,3), Nil:List[Int)(Cons(_,_)), should return List(1,2,3)"){
      val x = List.foldRight(List(1,2,3),Nil:List[Int])(Cons(_, _))
      assert(x == List(1,2,3))
    }

    it("the length of List(2,3,4,5) should be 4"){
      val len = List.length(List(2,3,4,5))
      assert(len == 4)
    }

    it("implement sum function using the function foldLeft()") {
      val sum = List.foldLeft(List(2,3,4),0.0)(_ + _)
      assert(sum == 9)
    }

    it("Test productByFoldLeft(List(5,6,10)) should return 300"){
      val product = List.productByFoldLeft(List(5,6,10))
      assert(product == 300)
    }

    it("test the length of List(34,55,90,12) using the method of lenghtByFoldLeft() should return 4 "){
      val len = List.lengthByFoldLeft(List(34,55,90,12))
      assert(len == 4)
    }

    it("test sum of List(23,42,50,59) using sumByFoldLeft() should return 174"){
      val sum = List.sumByFoldLeft(List(23,42,50,59))
      assert(sum==174)
    }

    it("test sum of List(23,42,50,59) using functional method sumByFoldLeftWithTailRec() should return 174"){
      val sum = List.sumByFoldLeftWithTailRec(List(23,42,50,59))
      assert(sum == 174)
    }

    it("test product of List(3,9,30) using functional method productFoldLeftWithTailRec() should return 810"){
      val product = List.productByFoldLeftWithTailRec(List(3,9,30))
      assert(product==810)
    }

    it("test length of List(2,355,9,304) using functional method lengthByFoldLeftWithTailRec() should return 4"){
      val len = List.lengthByFoldLeftWithTailRec(List(2,355,9,304))
      assert(len == 4)
    }

    it("reverse List(1,2,3) should return List(3,2,1)"){
      val reversedList = List.reverse(List(1,2,3))
      assert(reversedList == List(3,2,1))
    }

    it("List(1,2,3) appends List(4,5,6) should return List(1,2,3,4,5,6)"){
      val l1_l2 = List.appendByFoldRight(List(1,2,3))(List(4,5,6))
      assert(l1_l2 == List(1,2,3,4,5,6))
    }

    it("List[List(4,22),List(77,90] concat should return List(4,22,77,90)"){
      val concat = List.concat(List(List(4,22),List(77,90)))
      assert(concat == List(4,22,77,90))
    }

    it("each integer of the List(5,6,7) will be added one, should return List(6,7,8) "){
      val addedList = List.addOne(List(5,6,7))
      assert(addedList==List(6,7,8))
    }

    it("covert each element of List(45.4,89.9,32.7) to String" ){
      val stringList = List.doubleToString(List(45.4,89.9,32.7))
      assert(stringList==List("45.4","89.9","32.7"))
    }

    it("test map function List(1,2,3)"){
      val maped = List.map(List(1,2,3))(_ + 1)
      assert(maped == List(2,3,4))

      val toString = List.map(List(5,6,7))(_.toString)
      assert(toString == List("5","6","7"))
    }

  }

}
