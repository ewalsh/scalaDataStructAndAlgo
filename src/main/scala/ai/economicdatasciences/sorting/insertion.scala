package ai.economicdatasciences.dsa.sorting

object InsertionSortAscApp {
  def main(args: Array[String]): Unit = {
    println(insertionSortAsc(List(15, 10, 33, 11)))
    println(insertionSortAsc(List("banana", "apple", "mango")))
  }

  def insertionSortAsc[T <% Ordered[T]](myData: List[T]): List[T] = {
    if (myData == Nil) {
      myData
    } else {
      val head :: tail = myData
      val temp = insertionSortAsc(tail)
      insertElementAsc(head, temp)
    }
  }

  def insertElementAsc[T <% Ordered[T]](
      elem: T,
      sortedSubList: List[T]
  ): List[T] = {
    if (sortedSubList == Nil) {
      return elem :: sortedSubList
    } else {
      val head :: tail = sortedSubList
      if (head <= elem) {
        head :: insertElementAsc(elem, tail)
      } else {
        elem :: sortedSubList
      }
    }
  }
}
