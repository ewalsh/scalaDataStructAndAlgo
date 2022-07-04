package ai.economicdatasciences.dsa.sorting

object QuickSortAsc {
  def main(args: Array[String]): Unit = {
    println(quickSortAsc(List(5, 2, 1, 6, 7)))
    println(quickSortAsc(List("grape", "apple", "apricot")))
  }

  def quickSortAsc[T <% Ordered[T]](myData: List[T]): List[T] = myData match {
    case Nil => Nil
    case head :: Nil => List(head)
    case head :: tail => {
      val (p1, p2) = partitionAsc(head, tail, Nil, Nil)
      val leftToPivot = quickSortAsc(p1)
      val rightToPivot = quickSortAsc(p2)
      val tmp = head :: rightToPivot
      return leftToPivot ++ tmp
    }
  }

  def partitionAsc[T <% Ordered[T]](
      pivot: T,
      myData: List[T],
      p1: List[T],
      p2: List[T]
  ): (List[T], List[T]) = myData match {
    case Nil => (p1, p2)
    case head :: tail => {
      if (head < pivot) {
        partitionAsc(pivot, tail, head :: p1, p2)
      } else {
        partitionAsc(pivot, tail, p1, head :: p2)
      }
    }
  }
}
