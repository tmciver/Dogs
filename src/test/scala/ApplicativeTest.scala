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
    val addOneA: Maybe[Int => Int] = maybeApplicative.pure((_: Int) + 1)
    val doubleA: Maybe[Int => Int] = maybeApplicative.pure((i: Int) => i * 2)
    val xA = maybeApplicative.pure(1)
    def compose[A, B, C](f: A => B)(g: B => C): A => C = g compose f
    def composeA[A, B, C]: Maybe[(A => B) => (B => C) => (A => C)] = maybeApplicative.pure(compose[A, B, C] _)

    val left: Maybe[Int] =
      maybeApplicative.apply[Int, Int](
        //maybeApplicative.apply[(B => C, A => C](ff: F[(B => C) => (A => C)])(fa: F[B => C]): F[A => C]
        maybeApplicative.apply[Int => Int, Int => Int](
          // maybeApplicative.apply[A => B, (B => C) => (A => C)](ff: F[(A => B) => (B => C) => (A => C)])(fa: F[A => B]): F[(B => C) => (A => C)]
          maybeApplicative.apply[Int => Int, (Int => Int) => (Int => Int)](composeA[Int, Int, Int])(addOneA))(doubleA))(xA)

    val right: Maybe[Int] = maybeApplicative.apply(doubleA)(maybeApplicative.apply(addOneA)(xA))

    left should be (right)

  }
}
