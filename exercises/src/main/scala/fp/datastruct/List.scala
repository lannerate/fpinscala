package fp.datastruct


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


  val example = List(1,2,3)
  val another = Cons(6, Cons(5,Cons(4,Cons(3,Cons(2,Cons(1,Nil))))))
  val total = sum(example);

}
