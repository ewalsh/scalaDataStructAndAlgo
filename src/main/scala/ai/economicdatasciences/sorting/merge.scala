package ai.economicdatasciences.dsa.sorting

object MergeSortAscApp {
  def main(args: Array[String]): Unit = {
    println(mergeSortAsc(List(5, 6, 2, 3, 1)))
    println(mergeSortAsc(List("cat", "put", "bag")))
  }

  def mergeSortAsc[T <% Ordered[T]](myData: List[T]): List[T] = {
    if (myData == Nil || myData.tail == Nil) {
      return myData
    }

    val (myDataSplit1, myDataSplit2) = split(myData)
    val sortedSL1 = mergeSortAsc(myDataSplit1)
    val sortedSL2 = mergeSortAsc(myDataSplit2)
    mergeAsc(sortedSL1, sortedSL2)
  }

  def mergeAsc[T <% Ordered[T]](
      sortedSubList1: List[T],
      sortedSubList2: List[T]
  ): List[T] = (sortedSubList1, sortedSubList2) match {
    case (sortedSubList1, Nil) => sortedSubList1
    case (Nil, sortedSubList2) => sortedSubList2
    case (x1 :: y1, x2 :: y2) => {
      if (x1 > x2) {
        x2 :: mergeAsc(sortedSubList1, y2)
      } else {
        x1 :: mergeAsc(y1, sortedSubList2)
      }
    }
  }

  def split[T <% Ordered[T]](myData: List[T]): (List[T], List[T]) = {
    if (myData == Nil) {
      return (Nil, Nil)
    }

    val headOfMyData = myData.head
    val tailOfMyData = myData.tail
    if (tailOfMyData == Nil) {
      return (headOfMyData :: Nil, Nil)
    }
    val headOfTailOfMyData = tailOfMyData.head
    val tailOfTailOfMyData = tailOfMyData.tail
    val (tailOfTailOfMyDataSplit1, tailOfTailOfMyDataSplit2) = split(
      tailOfTailOfMyData
    )
    return (
      headOfMyData :: tailOfTailOfMyDataSplit1,
      headOfTailOfMyData :: tailOfTailOfMyDataSplit2
    )
  }
}
