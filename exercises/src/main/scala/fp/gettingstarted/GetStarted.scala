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
}
