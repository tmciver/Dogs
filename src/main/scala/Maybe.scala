package dogs

sealed trait Maybe[A] extends Functor[A]

object Maybe {

  case class Just[A](x: A) extends Maybe[A] {

    override def map[B](f: A => B) = {
      Just(f(x))
    }
  }
}
