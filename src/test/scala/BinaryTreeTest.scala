package dogs.test

import org.scalatest._
import dogs.BinaryTree._

class BinaryTreeTest extends FlatSpec with Matchers {

  "mapping toUpperCase over a Binarytree containing Strings" should
  "return a Binarytree containing upper cased versions of the original strings" in {

    val tree: BinaryTree[String] = Node(
      "programming",
      Node("functional", Leaf, Leaf),
      Node("rocks!", Leaf, Leaf)
    )
    val expectedTree: BinaryTree[String] = Node(
      "PROGRAMMING",
      Node("FUNCTIONAL", Leaf, Leaf),
      Node("ROCKS!", Leaf, Leaf)
    )
    val actualTree = binaryTreeFunctor.map(tree)(_.toUpperCase)
    actualTree should be (expectedTree)
  }
}
