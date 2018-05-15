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
    val fa: Maybe[Int => String] = maybeApplicative.pure(_.toString)
    val ga: Maybe[String => Boolean] = maybeApplicative.pure({
      case s if s.length > 0 => s(0).isUpper
      case _ => false
    })
    val xa = maybeApplicative.pure(1)
    def compose[A, B, C](f: A => B)(g: B => C): A => C = g compose f
    def composeA[A, B, C]: Maybe[(A => B) => (B => C) => (A => C)] = maybeApplicative.pure(compose[A, B, C] _)

    val left: Maybe[Boolean] =
      maybeApplicative.apply[Int, Boolean](
        //maybeApplicative.apply[(B => C, A => C](fa: F[(B => C) => (A => C)])(fa: F[B => C]): F[A => C]
        maybeApplicative.apply[String => Boolean, Int => Boolean](
          // maybeApplicative.apply[A => B, (B => C) => (A => C)](fa: F[(A => B) => (B => C) => (A => C)])(fa: F[A => B]): F[(B => C) => (A => C)]
          maybeApplicative.apply[Int => String, (String => Boolean) => (Int => Boolean)](composeA[Int, String, Boolean])(fa))(ga))(xa)

    val right: Maybe[Boolean] = maybeApplicative.apply(ga)(maybeApplicative.apply(fa)(xa))

    left should be (right)

  }
}
