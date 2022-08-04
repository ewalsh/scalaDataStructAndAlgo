package ai.economicdatasciences.dsa.bintree

sealed trait BinaryTree[+Int]
case object Leaf extends BinaryTree[Nothing]
case class Branch(value: Int, left: BinaryTree[Int], right: BinaryTree[Int]) extends BinaryTree[Int]

class ExampleBinTree(binTree: BinaryTree[Int]) {
  // depth cache
  var dcatch = List[(Int, Int)]()

  def collectDepthFirst(
    node: BinaryTree[Int],
    visited: List[(Int, Int)],
    currentDepth: Int = 0
  ): Unit = node match {
    case Leaf => {
      dcatch = (currentDepth, 0) +: dcatch
    }
    case Branch(v, l, r) => {
      dcatch = (currentDepth, v) +: dcatch
      collectDepthFirst(
        l,
        List[(Int, Int)](),
        currentDepth + 1
      )
      collectDepthFirst(
        r,
        List[(Int, Int)](),
        currentDepth + 1
      )

    }
  }

}

object ExcampleBinTreeApp {
  def main(args: Array[String]): Unit = {
    val myList = List(1,2,3,4,5,6)
    val myBinTree = createTree(myList)
    val exp = new ExampleBinTree(myBinTree)
    val test = exp.collectDepthFirst(myBinTree, List[(Int, Int)]())
    println(exp.dcatch.groupBy(_._1).map(x => (x._1, x._2.map(_._2).sum / x._2.length)))
  }

  def createTree(list: List[Int]): BinaryTree[Int] = list match {
    case Nil => Leaf
    case x :: xs => {
      val hl = xs.length / 2
      Branch(
        x,
        createTree(xs.take(hl)),
        createTree(xs.drop(hl))
      )
    }
  }
}
