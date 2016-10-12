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
    it("test sizeByFold() for branch(branch1,branch2)"){
      val size = Tree.sizeByFold(mockData)
      assert(size == 7)
    }

    it("test maximum() for branch(branch1,branch2)") {
      val max = Tree.maximum(mockData)
      assert(max == 4)
    }

    it("test maximumByFold() for branch(b1,b2)"){
      val max = Tree.maximumByFold(mockData)
      assert(max == 4)
    }

    it("the depth of branch(branch1,branch2) should equal 2") {
      val depth = Tree.depth(mockData)
      assert(depth == 2)
    }

    it("the depth of branch(b1,b2) should equal 2"){
      val depth = Tree.depthByFold(mockData)
      assert(depth == 2)
    }

    it("test the function map()"){
      val mapedTree = Tree.map( Branch(Leaf(3),Leaf(4)) )( _ % 2 == 0)

      assert(mapedTree == Branch(Leaf(false),Leaf(true)))
    }

    it("the function mapByFold()"){
      val mapedTree = Tree.mapByFold( Branch(Leaf(4),Leaf(5) ) ) ( _ * 2)

      assert(mapedTree == Branch(Leaf(8),Leaf(10)))
    }
  }
}
