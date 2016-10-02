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

  }

}
