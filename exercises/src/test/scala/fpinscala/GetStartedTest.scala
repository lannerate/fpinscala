package fpinscala

import org.scalatest.FlatSpec
import fpinscala.gettingstarted.MyModule._

/**
  * Created by apple on 9/23/16.
  */
class GetStartedTest extends FlatSpec{

  "Fib function " should "0,1,1,2,3,5,8,13" in {
    assert( fib(0) === 0 )
    assert( fib(1) === 1 )
    assert( fib(2) === 1 )
    assert( fib(3) === 2 )
    assert( fib(4) === 3 )
    assert( fib(5) === 5 )
    assert( fib(6) === 8 )
    assert( fib(7) === 13 )
  }
}
