package dogs

trait Functor[A] {

  def map[B, T <: Functor[B]](f: A => B): T
}
