package fpinscala

import org.scalatest.FlatSpec
import fp.gettingstarted.GetStarted._
import fp.gettingstarted.{MonomorphicBinarySearch, PolymorphicBinarySearch}

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

  "Print test result" should " factorial(n) = n! " in {
    assertResult("The factorial(n) of 1 is 1")(formatResult("factorial(n)",1, factorial ) );
    assertResult("The factorial(n) of 2 is 2")(formatResult("factorial(n)",2, factorial ) );
    assertResult("The factorial(n) of 3 is 6")(formatResult("factorial(n)",3, factorial ) );
    assertResult("The factorial(n) of 4 is 24")(formatResult("factorial(n)",4, factorial ) );
    assertResult("The factorial(n) of 5 is 120")(formatResult("factorial(n)",5, factorial ) );
  }

  "Less than function " should " 12 < 20 " in {
    assert(lessThan(12,20))
    assert(lessThan(11,20))
  }

  "Monomorphic Binary Search 99.3" should " at index(2) " in {
    assertResult(2)(MonomorphicBinarySearch.binarySearch(Array(23.23,33.56,99.3,102.2,75.3,90.8),99.3))
  }

  "Polymorphic Binary Search 99.3" should " at index(2) using double Type " in {
     val result = PolymorphicBinarySearch.binarySearch(
      Array(23.23,33.56,99.3,102.2,75.3,90.8),
      99.3,
      (d1:Double,d2:Double) => d1 > d2)
    assert( result == 2 )
  }

  "Polymorphic Binary Search 99 " should " at index(3) using double Type " in {
     val result = PolymorphicBinarySearch.binarySearch(
       Array(23,33,99,102,75,90,78),
      99,
      (d1:Int,d2:Int) => d1 > d2)

    assert( result == 2 )
  }
}
