package ai.economicdatasciences.dsa.graphs

object TopologicalSortingApp {
  def main(args: Array[String]): Unit = {
    val eventsList = List(
      ("prepare_test", "take_test"),
      ("apply", "receive_offer"),
      ("receive_offer", "join_CMU"),
      ("join_CMU", "choose_major"),
      ("choose_major", "graduate"),
      ("receive_offer", "join_MIT"),
      ("join_MIT", "choose_major"),
      ("choose_major", "graduate")
    )

    println(topologicalSort(eventsList))
  }

  def topologicalSort(graph: List[(String, String)]) = {
    def topSort(vertices: List[String], visited: List[String]): List[String] =
      vertices match {
        case Nil => visited
        case x :: xs => {
          topSort(
            xs,
            if (visited.contains(x)) {
              visited
            } else {
              x :: topSort(calcSuccessorSet(x, graph), visited)
            }
          )
        }
      }

    val (start, _) = graph.unzip
    val result = topSort(start, List())
    result
  }

  def calcSuccessorSet(
      vertex: String,
      graph: List[(String, String)]
  ): List[String] = graph match {
    case Nil => Nil
    case x :: xs if (vertex == x._1) => x._2 :: calcSuccessorSet(vertex, xs)
    case _ :: xs => calcSuccessorSet(vertex, xs)
  }
}
