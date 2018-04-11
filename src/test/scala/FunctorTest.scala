package dogs.test

import org.scalatest._
import dogs.Maybe._

class FunctorTest extends FlatSpec with Matchers {

  "A Functor" should "not change when mapping the identity function." in {
    val functor = Just(5)
    functor.map(identity) should be (functor)
  }

  "Successively mapping a Functor over two functions" should
  "be the same as mapping the composition of the two functions." in {
    val functor = Just(5)
    val addOne = (i: Int) => i + 1
    val double = (i: Int) => i * 2

    functor.map(addOne).map(double) should be (functor.map(addOne andThen double))
  }
}
