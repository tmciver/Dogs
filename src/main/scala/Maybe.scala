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

  val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {
    def pure[A](v: A): Maybe[A] = Just(v)
    def apply[A, B](ff: Maybe[A => B])(maybe: Maybe[A]): Maybe[B] = ff match {
      case Just(f) => maybe match {
        case Just(x) => Just(f(x))
        case None => None
      }
      case None => None
    }
  }
}
