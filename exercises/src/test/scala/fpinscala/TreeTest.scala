package fpinscala

import hui.fp.datastruct.{Branch, Leaf, Tree}
import org.scalatest.FunSpec

/**
  * Created by hzhang3 on 10/12/2016.
  */
class TreeTest extends FunSpec {

  describe("Test Tree cases") {

    def mockData: Branch[Int] = {
      val l = Leaf(1)
      val r = Leaf(2)
      val branch1 = Branch(l, r)

      val l2 = Leaf(3)
      val r2 = Leaf(4)
      val branch2 = Branch(l2, r2)

      Branch(branch1, branch2)
    }

    it("test size() of branch(branch1,branch2)") {

      val size = Tree.size(mockData)

      assert(size == 7)

    }

    it("test maximum() for branch(branch1,branch2)"){
      val max = Tree.maximum(mockData)
      assert(max == 4)

    }
  }
}
