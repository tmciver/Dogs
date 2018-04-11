package dogs

trait Functor[A] {
  type Self[Z] <: Functor[Z]

  def map[B](f: A => B): Self[B]
}
