package ai.economicdatasciences.dsa.binarytree.springer

sealed trait BinaryTree[+A]
case object Leaf extends BinaryTree[Nothing]
case class Branch[A](value: A, left: BinaryTree[A], right: BinaryTree[A])
    extends BinaryTree[A]

object BinaryTreeApp {
  def main(args: Array[String]): Unit = {
    val myList = List(1, 2, 3, 4, 5, 6)
    val myBinTree = createTree(myList)
    println(myBinTree)
    println(size(myBinTree))
    println(depth(myBinTree))
  }

  def createTree[A](list: List[A]): BinaryTree[A] = list match {
    case Nil => Leaf
    case x :: xs => {
      val halfLength = xs.length / 2
      Branch(
        x,
        createTree(xs.take(halfLength)),
        createTree(xs.drop(halfLength))
      )
    }
  }

  def size[A](binTree: BinaryTree[A]): Int = binTree match {
    case Leaf => 0
    case Branch(_, leftBranch, rightBranch) =>
      1 + size(leftBranch) + size(rightBranch)
  }

  def depth[A](binTree: BinaryTree[A]): Int = binTree match {
    case Leaf => 0
    case Branch(_, leftBranch, rightBranch) =>
      1 + scala.math.max(depth(leftBranch), depth(rightBranch))
  }
}

object BinTreeTraversal {
  def main(args: Array[String]): Unit = {
    println("WIP")
  }

  def depth[A](binTree: BinaryTree[A]): Int = binTree match {
    case Leaf => 0
    case Branch(_, leftBranch, rightBranch) =>
      1 + scala.math.max(depth(leftBranch), depth(rightBranch))
  }

  def preorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) => {
      value :: (preorder(leftBranch) ++ preorder(rightBranch))
    }
  }

  def inorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) => {
      inorder(leftBranch) ++ (value :: inorder(rightBranch))
    }
  }

  // def preorderWithDepth[A](binTree: BinaryTree[A]): (Int, A) = binTree match {
  //   case Leaf => (depth(binTree), Nil)
  //   case Branch(v, l, r) => {
  //     (depth(binTree), v :: (preorderWithDepth(l) ++ preorderWithDepth(r)))
  //   }
  // }
}
