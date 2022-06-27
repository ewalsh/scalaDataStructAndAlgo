package ai.economicdatasciences.dsa.binarytree

/** */

sealed trait BinaryTree[+A]
case object Leaf extends BinaryTree[Nothing]
case class Branch[A](value: A, left: BinaryTree[A], right: BinaryTree[A])
    extends BinaryTree[A]

object TreeBuilder {
  // add binary tree methods
  def buildBalancedTree[A](list: List[A]): BinaryTree[A] = list match {
    case Nil => Leaf
    case x :: xs => {
      val k = xs.length / 2
      Branch(x, buildBalancedTree(xs.take(k)), buildBalancedTree(xs.drop(k)))
    }
  }

  def size[A](tree: BinaryTree[A]): Int = tree match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + size(l) + size(r)
  }

  def depth[A](tree: BinaryTree[A]): Int = tree match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + scala.math.max(depth(l), depth(r))
  }
}
