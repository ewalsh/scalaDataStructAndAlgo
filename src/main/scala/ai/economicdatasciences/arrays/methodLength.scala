package ai.economicdatasciences.dsa.arrays

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

object MethodLengthApp {
  def main(args: Array[String]): Unit = {
    println(calcLength(Array('a', 'e', 'i', 'o', 'u', '\0')))
  }

  def calcLength(arr: Array[Char]): Int = {
    @tailrec
    def attempt(iter: Int, arr: Array[Char]): Int = {
      val attempted = Try {
        arr(iter)
      }
      attempted match {
        case Success(_) => attempt(iter + 1, arr)
        case Failure(_) => iter - 1
      }
    }
    attempt(0, arr)
  }
}
