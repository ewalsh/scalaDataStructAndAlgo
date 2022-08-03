package ai.economicdatasciences.dsa.search

object NaiveSubstringSearchApp {
  def main(args: Array[String]): Unit = {
    val myData1 = "This is a functional implementation functional is functional"
    val myWords1 = "functional"
    println(naiveSubstringSearch(myWords1, myData1))
  }

  def naiveSubstringSearch(myWords: String, myData: String): Int = {
    myData.indices
      .find(i => {
        i + myWords.length <= myData.length && myWords.indices.forall(j => {
          myData(j + i) == myWords(j)
        })
      })
      .getOrElse(-1)
  }

  def naiveSubstringSearchAll(myWords: String, myData: String): List[Int] = {
    def searchAll(myWords: String, myDataArr: Array[Char], output: List[Int]): List[Int] = {
      val subDataString: String = myDataArr.slice(
        output.foldLeft(0)(_ + _),
        myDataArr.length
      ).mkString
      val wordId = naiveSubstringSearch(myWords, subDataString)
      wordId match {
        case -1 => {
          println("output")
          output
        }
        case i => {
          println(i + myWords.length - 1)
          searchAll(
            myWords,
            myDataArr,
            (i + myWords.length - 1) +: output
          )
        }
      }
    }

    searchAll(myWords, myData.toArray, List[Int]())
  }
}
