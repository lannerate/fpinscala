package hui.fp.datastruct

/**
  * Created by hzhang3 on 10/12/2016.
  */

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](nodes: Tree[A]): Int = nodes match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(nodes:Tree[Int]):Int = nodes match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](nodes:Tree[A]):Int = nodes match {
    case Leaf(_) => 0
    case Branch(l,r) => ( depth(l) max depth(r) ) + 1
  }
}
