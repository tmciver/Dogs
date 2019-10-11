package dogs.test

import org.scalatest._
import dogs.Maybe._
import dogs.BinaryTree._

class FunctorTest extends FlatSpec with Matchers {

  "A Functor" should "not change when mapping the identity function." in {
    val maybe = Just(5)
    maybeFunctor.map(maybe)(identity) should be (maybe)
  }

  "Successively mapping a Functor over two functions" should
  "be the same as mapping the composition of the two functions." in {
    val maybe = Just(5)
    val addOne = (i: Int) => i + 1
    val double = (i: Int) => i * 2

    maybeFunctor.map(maybeFunctor.map(maybe)(addOne))(double) should be (maybeFunctor.map(maybe)(addOne andThen double))

  }

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
