package ai.economicdatasciences.dsa.binarytree

/** */

sealed trait BinaryTree[+A]
case object Leaf extends BinaryTree[Nothing]
case class Branch[A](value: A, left: BinaryTree[A], right: BinaryTree[A])
    extends BinaryTree[A]

object TreeBuilder {
  // types
  type Dictionary[A] = BinaryTree[(String, A)]
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

  def equal[A](tree1: BinaryTree[A], tree2: BinaryTree[A]): Boolean =
    (tree1, tree2) match {
      case (Leaf, Leaf) => true
      case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if v1 == v2 => {
        equal(l1, l2) && equal(r1, r2)
      }
      case _ => false
    }

  def flip[A](tree: BinaryTree[A]): BinaryTree[A] = tree match {
    case Leaf => Leaf
    case Branch(v, l, r) => Branch(v, flip(r), flip(l))
  }

  def flippedEqual[A](tree1: BinaryTree[A], tree2: BinaryTree[A]): Boolean =
    (tree1, tree2) match {
      case (Leaf, Leaf) => true
      case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if v1 == v2 => {
        flippedEqual(l1, r2) && flippedEqual(l2, r1)
      }
      case _ => false
    }

  def preorder[A](tree: BinaryTree[A]): List[A] = tree match {
    case Leaf => Nil
    case Branch(v, l, r) => v :: (preorder(l) ++ preorder(r))
  }

  def inorder[A](tree: BinaryTree[A]): List[A] = tree match {
    case Leaf => Nil
    case Branch(v, l, r) => (inorder(l) ++ (v :: inorder(r)))
  }

  def postorder[A](tree: BinaryTree[A]): List[A] = tree match {
    case Leaf => Nil
    case Branch(v, l, r) => postorder(l) ++ postorder(r) ++ List(v)
  }

  // accumulator
  def preorderAcc[A](tree: BinaryTree[A], acc: List[A]): List[A] = tree match {
    case Leaf => acc
    case Branch(v, l, r) => {
      println(s"When at ${v} - acc = ${acc}")
      v :: preorderAcc(l, preorderAcc(r, acc))
    }
  }

  // placeholder for more accumulator methods -- skipping for now

  // methods for binary search tree
  def insert[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] =
    dict match {
      case Leaf => Branch((key, value), Leaf, Leaf)
      case Branch((k, v), l, r) if (k == key) =>
        sys.error(s"key {key} is already present")
      case Branch((k, v), l, r) if (k > key) =>
        Branch((k, v), insert(key, value, l), r)
      case Branch((k, v), l, r) if (k < key) =>
        Branch((k, v), l, insert(key, value, r))
    }

  // searching key
  def search[A](key: String, dict: Dictionary[A]): Option[A] = dict match {
    case Leaf => None
    case Branch((k, v), l, r) if (k == key) => Some(v)
    case Branch((k, v), l, r) if (k > key) => search(k, l)
    case Branch((k, v), l, r) if (k < key) => search(k, r)
  }

  def update[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] =
    dict match {
      case Leaf => Branch((key, value), Leaf, Leaf)
      case Branch((k, v), l, r) if (k == key) => Branch((k, value), l, r)
      case Branch((k, v), l, r) if (k > key) =>
        Branch((k, value), update(key, value, l), r)
      case Branch((k, v), l, r) if (k < key) =>
        Branch((k, value), l, update(key, value, r))
    }

  // create empty
  def empty[A](): Dictionary[A] = Leaf

}
