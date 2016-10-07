package fp.datastruct

import scala.annotation.tailrec


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A] (head:A, tail:List[A]) extends List[A]

object List {

  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds:List[Double]):Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head,apply(as.tail: _*))

  def tail[A](as: List[A]):List[A] =
    as match {
      case Nil => sys.error("tail is empty")
      case Cons(_, t) => t
    }

  def drop[A](l:List[A], n:Int):List[A] = {
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t,n-1)
    }
  }

  def dropWhile[A](l:List[A])(f: A=>Boolean ):List[A] = {
    l match {
      case Cons(h,t) if(f(h)) => dropWhile(t)(f)
      case _ => l
    }
  }

  def setHead[A](l:List[A])(h:A):List[A] ={
    l match {
      case Nil => sys.error("the list is empty")
      case Cons(_, t) => Cons(h,t)
    }
  }

  def append[A](a1:List[A],a2:List[A]):List[A] ={
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h,append(t,a2))
    }
  }

  def appendByFoldRight[A](l1:List[A])(l2:List[A]):List[A] = foldRight(l1,l2)( Cons(_,_) )

  def concat[A](l:List[List[A]]):List[A] = foldRight(l, Nil:List[A])(append)

  def addOne(l:List[Int]):List[Int] = foldRight(l,Nil:List[Int])( (h,t) => Cons(h+1,t) )

  def doubleToString(l:List[Double]):List[String] = foldRight(l,Nil:List[String])((h,t) => Cons(h.toString,t))

  def map[A,B](l:List[A])(f:A=>B):List[B] = foldRight(l,Nil:List[B])( (h,t) => Cons(f(h),t) )

  def flatMap[A,B](l:List[A])(f:A=>List[B]):List[B] = concat(map(l)(f))

  def filter[A](l:List[A])(f:A=>Boolean):List[A] = foldRight(l,Nil:List[A])( (h,t) => if(f(h)) Cons(h,t) else t )

  def filterByFlatMap[A](l:List[A])(f:A=>Boolean):List[A] = flatMap(l)(a => if(f(a)) List(a) else Nil )

  def addPair(l1:List[Int],l2:List[Int]):List[Int] = (l1,l2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2, addPair(t1,t2))
  }

  def init[A](l:List[A]):List[A] = {
    l match {
      case Nil => sys.error("the list is empty")
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  }

  def foldRight[A, B](l:List[A],z:B)(f:(A,B)=>B):B = {
    l match {
      case Nil => z
      case Cons(x,xs) => f(x,foldRight(xs,z)(f))
    }
  }

  @tailrec
  def foldLeftWitTailRec[A,B](l:List[A], z:B)(f:(B,A)=>B):B ={
    l match {
      case Nil => z
      case Cons(h,t) => foldLeftWitTailRec(t,f(z,h))(f)
    }
  }

  def reverse[A](l:List[A]):List[A] = foldLeftWitTailRec(l,List[A]())( (h,acc) => Cons(acc, h) )

  def sumByFoldLeftWithTailRec(l:List[Int]) = foldLeftWitTailRec(l,0.0)(_ + _)

  def productByFoldLeftWithTailRec(l:List[Double]) = foldLeftWitTailRec(l, 1.0)(_ * _)

  def lengthByFoldLeftWithTailRec[A](l:List[A]):Int = foldLeftWitTailRec(l,0)((acc, _) => acc + 1)

  def foldLeft[A,B](l:List[A], z:B)(f:(B,A)=>B):B = {
    l match {
      case Nil => z
      case Cons(x,xs) => f( foldLeft(xs,z)(f),x )
    }
  }

  def sumByFoldLeft(l:List[Double]) = foldLeft(l, 0.0)(_ + _)

  def productByFoldLeft(l:List[Double]) = foldLeft(l, 1.0)(_ * _)

  def lengthByFoldLeft[A](l:List[A]):Int = foldLeft(l,0) ((acc,_) => acc + 1)

  def sum2(l:List[Int]) = foldRight(l, 0.0)(_ + _)

  def product2(l:List[Double]) = foldRight(l,1.0)(_ * _)

  def length[A](l:List[A]): Int = foldRight(l,0) ( (_,acc) => acc + 1 )

  val example = List(1,2,3)
  val another = Cons(6, Cons(5,Cons(4,Cons(3,Cons(2,Cons(1,Nil))))))
  val total = sum(example);

}
