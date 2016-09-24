package fpinscala

import org.scalatest.FlatSpec
import fp.gettingstarted.GetStarted._

/**
  * Created by apple on 9/23/16.
  */
class GetStartedTest extends FlatSpec{

  "Fib function " should " satisfy f(n) = f(n-1) + f(n-2)" in {
    assert( fib(0) == 0 )
    assert( fib(1) == 1 )
    assert( fib(2) == 1  )
    assert( fib(3) == 1 + 1)
    assert( fib(4) == 2 + 1 )
    assert( fib(5) == ( 2+1 ) + ( 1 + 1 ) )
    assert( fib(6) == ( ( 2+1 ) + ( 1 +1 ) ) + ( 2 + 1 ) )
    assert( fib(7) == fib(6) + fib(5) )
  }

  "Factorial function" should " satisfy f(n) = n! = n*(n-1)*(n-1)*...3*2*1 " in {
    assume(factorial(1) == 1)
    assume(factorial(2) == 2*1)
    assume(factorial(3) == 3*2*1)
    assume(factorial(4) == 4*3*2*1)
    assume(factorial(5) == 5*4*3*2*1)
  }
}
