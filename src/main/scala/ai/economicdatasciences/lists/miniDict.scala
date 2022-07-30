package ai.economicdatasciences.dsa.lists

import scala.annotation.tailrec

class MiniDictionary {
  val dictionary = List("apple", "cow", "color", "god", "goat", "dog", "house", "mother", "orange", "rat", "zeal", "university", "orange")

  // for practice write alternative to .sorted
  def countByFirstLetter: Map[Char, Int] = {
    val charList = dictionary.map(_(0))
    val charGrps = charList.zip(dictionary).groupBy(_._1).map(x => (x._1 -> x._2.map(_._2).length))
    charGrps
  }

  def returnSorted: List[String] = {
    dictionary.sorted
  }

  def containsDuplicate: Boolean = {
    dictionary.length != dictionary.distinct.length
  }

  // return dupliated word
  def returnDuplicates: List[String] = {
    @tailrec
    def go(iter: Int, out: List[String]): List[String] = {
      iter match {
        case 0 => {
          dictionary.slice(0, iter).contains(dictionary(iter)) match {
            case true => {
              out :+ dictionary(iter)
            }
            case false => out
          }
        }
        case _ => {
          dictionary.slice(0, iter).contains(dictionary(iter)) match {
            case true => {
              go(iter - 1, out :+ dictionary(iter))
            }
            case false => out
          }
        }
      }
    }

    go(dictionary.length - 1, List[String]())
  }

  def numWords: Int = {
    dictionary.length
  }

  def checkWordExists(word: String): Boolean = {
    dictionary.exists(_ == word)
  }
}
