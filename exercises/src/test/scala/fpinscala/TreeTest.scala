package fpinscala

import hui.fp.datastruct.{Branch, Leaf, Tree}
import org.scalatest.FunSpec

/**
  * Created by hzhang3 on 10/12/2016.
  */
class TreeTest extends FunSpec{

  describe("Test Tree cases"){

    it("the functional size() of branch(leaf,leaf)"){
     val l = Leaf(1); val r = Leaf(2) ; val branch = Branch(l,r)

     val size = Tree.size( branch )

     assert(size == 3)
    }
  }
}
