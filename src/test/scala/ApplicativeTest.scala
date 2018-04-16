package dogs.test

import org.scalatest._
import dogs.Maybe._

class ApplicativeTest extends FlatSpec with Matchers {

  "An Applicative" should "not change when mapping the identity function." in {
    val maybeF = Just((x: Int) => x)
    val maybeVal = Just(5)
    maybeApplicative.apply(maybeF)(maybeVal) should be (maybeVal)
  }

  "An Applicative" should "satisfy the homomorphism law." in {
    val x = 1
    val addOne = (i: Int) => i + 1

    val pureX = maybeApplicative.pure(x)
    val pureF = maybeApplicative.pure(addOne)
    maybeApplicative.apply(pureF)(pureX) should be (maybeApplicative.pure(addOne(x)))
  }

  "An Applicative" should "satisfy the interchange law." in {
    val x = 1
    val addOne = (i: Int) => i + 1
    val xA = maybeApplicative.pure(x)
    val fA = maybeApplicative.pure(addOne)
    val applyToXA = maybeApplicative.pure((f: Int => Int) => f(x))

    maybeApplicative.apply(fA)(xA) should be (maybeApplicative.apply(applyToXA)(fA))
  }

  "An Applicative" should "satisfy the composition law." in {
    val addOneA = maybeApplicative.pure((_: Int) + 1)
    val doubleA = maybeApplicative.pure((i: Int) => i * 2)
    val xA = maybeApplicative.pure(1)
    def compose[A, B, C](f: A => B)(g: B => C): A => C = g compose f
    val composeA = maybeApplicative.pure(compose _)

    maybeApplicative.apply(composeA)(addOneA) should be (maybeApplicative.pure(addOne(x)))
    // The above gives error:
    // [error]  found   : dogs.Maybe.Maybe[Int => Int]
    // [error]  required: dogs.Maybe.Maybe[Nothing => Nothing]
    // [error]     maybeApplicative.apply(composeA)(addOneA) should be (maybeApplicative.pure(addOne(x)))
  }
}
