package dogs.test

import org.scalatest.{ FlatSpec, Matchers }
import dogs.BinaryTree
import dogs.BinaryTree._

class BinaryTreeTest extends FlatSpec with Matchers {

  "mapping toUpperCase over a Binarytree containing Strings" should
  "return a Binarytree containing upper cased versions of the original strings" in {

    val tree: BinaryTree[String] = insert("rocks!", insert("functional", insert("programming", BinaryTree.empty)))
    val expectedTree: BinaryTree[String] = insert("ROCKS!", insert("FUNCTIONAL", insert("PROGRAMMING", BinaryTree.empty)))

    val actualTree = binaryTreeFunctor.map(tree)(_.toUpperCase)
    actualTree should be (expectedTree)
  }
}
