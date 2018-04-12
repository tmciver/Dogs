package dogs

object Maybe {

  sealed trait Maybe[+A]
  case class Just[A](x: A) extends Maybe[A]
  case object None extends Maybe[Nothing]

  val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match {
      case Just(v) => Just(f(v))
      case None => None
    }
  }
}
