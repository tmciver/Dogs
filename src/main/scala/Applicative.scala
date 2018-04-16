package dogs

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](v: A): F[A]
  def apply[A, B](ff: F[A => B])(fa: F[A]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(pure(f))(fa)
}
