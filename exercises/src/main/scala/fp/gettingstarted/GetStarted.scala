package fp.gettingstarted

import scala.annotation.tailrec

/**
  * Created by apple on 9/24/16.
  */
object GetStarted {

  def factorial(n: Int): Int = {
    @tailrec
    def go(n:Int, acc:Int): Int ={
      if(n <= 0) acc
      else go(n-1, n * acc)
    }

    go(n,1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n:Int, pre:Int, cur:Int ): Int =
      if(n == 0) pre
      else loop(n-1, cur, pre + cur)
    loop(n,0,1)
  }

   def fib2(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println( formatResult("factorial(1)",1,factorial) )
    println( formatResult("increment1",4, (x:Int) => x + 1 ) )
    println( formatResult("increment2",4, (x) => x + 2 ) )
    println( formatResult("increment3",4,  x => x + 3 ) )
    println( formatResult("increment4",4,  _ + 4 ) )
    println( formatResult("increment5",4,  x => { val r = x + 5; r}) )

    val b = lessThan.apply(12,30);
  }

  def formatResult(name:String, n:Int, f: Int => Int ) = {
    val msg = "The %s of %d is %d"
    msg.format(name,n,f(n))
  }

  val lessThan = new Function2[Int,Int,Boolean] {
    override def apply(v1: Int, v2: Int): Boolean = v1 < v2
  }
}

object PolymorphicBinarySearch {

  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

}

object MonomorphicBinarySearch{

  def binarySearch(as:Array[Double], key:Double):Int = {
    @tailrec
    def go(low:Int,mid:Int,high:Int):Int = {
      if(low > high) -mid - 1
      else{
        val mid2 = (low + high)/2
        val d = as(mid2)
        if (d == key) mid2
        else if(d > key) go(low,mid2,mid2-1)
        else go(mid2+1, mid2, high)
      }
    }
    go(0,0,as.length-1)
  }
}

