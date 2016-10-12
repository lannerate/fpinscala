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

  def sizeByFold[A](t:Tree[A]):Int = fold(t)(_ => 1 :Int)( _ + _ + 1)

  def maximum(nodes:Tree[Int]):Int = nodes match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def maximumByFold(t:Tree[Int]):Int = fold(t)(n => n:Int)(_ max _)

  def depth[A](nodes:Tree[A]):Int = nodes match {
    case Leaf(_) => 0
    case Branch(l,r) => ( depth(l) max depth(r) ) + 1
  }

  def depthByFold[A](t:Tree[A]):Int = fold(t)(_ => 0)( _ max _ + 1  )

  def fold[A, B](t:Tree[A])(f :A=>B)(g :(B,B)=>B) :B = t match {
    case Leaf(n) => f(n)
    case Branch(l,r) => g( fold(l)(f)(g), fold(r)(f)(g) )
  }

  def map[A,B](nodes:Tree[A])(f: A => B):Tree[B] = nodes match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def mapByFold[A,B](nodes:Tree[A])(f: A=>B):Tree[B] =
    fold(nodes)(a => Leaf(f(a)):Tree[B])(Branch(_,_))

}
