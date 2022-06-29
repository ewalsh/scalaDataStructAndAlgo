package ai.economicdatasciences.dsa.graphs

import scala.annotation.tailrec

object GraphAlgo {
  // types
  type VC = (List[String], List[String])
  // testing graph
  val graph0 = List(
    ("m", "n"),
    ("m", "o"),
    ("m", "p"),
    ("n", "q"),
    ("o", "r"),
    ("p", "q"),
    ("q", "r"),
    ("q", "s")
  )

  val grwork = List(
    ("getup", "shower"),
    ("shower", "breakfast"),
    ("breakfast", "dress"),
    ("dress", "office"),
    ("office", "dinner"),
    ("breakfast", "leisurely_lunch"),
    ("leisurely_lunch", "movie"),
    ("movie", "dinner")
  )

  // a slow method to reverse a list
  def slowRev(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case x :: xs => slowRev(xs) :+ x
  }

  @tailrec
  def rev(list: List[Int], acc: List[Int] = Nil): List[Int] = list match {
    case Nil => {
      // println(s"acc = ${acc}")
      acc
    }
    case x :: xs => {
      // println(s"x = ${x}")
      // println(s"xs = ${xs}")
      // println(s"acc = ${acc}")
      rev(xs, x :: acc)
    }
  }

  def succSet(a: String, g: List[(String, String)]): List[String] = g match {
    case Nil => Nil
    case x :: xs if (a == x._1) => x._2 :: succSet(a, xs)
    case _ :: xs => succSet(a, xs)
  }

  // depth first traversal
  def depthFirst(initial: String, g: List[(String, String)]): List[String] = {
    def depthf(nodes: List[String], visited: List[String]): List[String] =
      nodes match {
        case Nil => visited
        case x :: xs if visited.contains(x) => depthf(xs, visited)
        case x :: xs => depthf(succSet(x, g) ++ xs, x :: visited)
      }

    val result = depthf(List(initial), List())
    result.reverse
  }

  // depth first with less appending
  def depthFirst1(initial: String, g: List[(String, String)]): List[String] = {
    def depthf(nodes: List[String], visited: List[String]): List[String] =
      nodes match {
        case Nil => visited
        case x :: xs => {
          depthf(
            xs,
            if (visited.contains(x)) visited
            else depthf(succSet(x, g), x :: visited)
          )
        }
      }

    val result = depthf(List(initial), List())
    result.reverse
  }

  // top sorting
  def topsort(g: List[(String, String)]) = {
    def sort(nodes: List[String], visited: List[String]): List[String] =
      nodes match {
        case Nil => visited
        case x :: xs => {
          sort(
            xs,
            if (visited.contains(x)) visited
            else x :: sort(succSet(x, g), visited)
          )
        }
      }

    val (start, _) = g.unzip
    val result = sort(start, List())
    result
  }

  // helper methods for cycles
  def addToVisited(x: String, v: VC) = (x :: v._1, v._2)

  def topsortPrintCycle(g: List[(String, String)]) = {
    def sort(nodes: List[String], path: List[String], visited: VC): VC =
      nodes match {
        case Nil => visited
        case x :: xs => {
          val (v, c) = visited
          sort(
            xs,
            path,
            if (path.contains(x)) (v, x :: c)
            else if (v.contains(x)) visited
            else addToVisited(x, sort(succSet(x, g), x :: path, visited))
          )
        }
      }

    val (start, _) = g.unzip
    val result = sort(start, List(), (List(), List()))
    result
  }

}
