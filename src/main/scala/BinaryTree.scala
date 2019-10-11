package dogs

object BinaryTree {

  sealed trait BinaryTree[+A]
  private case class Node[A](v: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]
  private case object Leaf extends BinaryTree[Nothing]

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

  val empty: BinaryTree[Nothing] = Leaf

  def insert[A: Ordering](v: A, t: BinaryTree[A]): BinaryTree[A] = t match {
    case n@Node(w, left, right) => {
      if (Ordering[A].lt(v, w)) {
        // insert into left sub-tree
        val newLeft = insert(v, left)
        Node(w, newLeft, right)
      } else if (Ordering[A].gt(v, w)) {
        // insert into right sub-tree
        val newRight = insert(v, right)
        Node(w, left, newRight)
      } else {
        n
      }
    }
    // Insert a new node in place of the Leaf.
    case Leaf => Node(v, Leaf, Leaf)
  }
}
