package dogs

object BinaryTree {

  sealed trait BinaryTree[+A]
  case class Node[A](v: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]
  case object Leaf extends BinaryTree[Nothing]

  val binaryTreeFunctor: Functor[BinaryTree] = new Functor[BinaryTree] {
    def map[A, B](fa: BinaryTree[A])(f: A => B): BinaryTree[B] = fa match {
      case Node(v, left, right) => {
        val newLeft = binaryTreeFunctor.map(left)(f)
        val newRight = binaryTreeFunctor.map(right)(f)
        val newV = f(v)
        Node(newV, newLeft, newRight)
      }
      case Leaf => Leaf
    }
  }
}
