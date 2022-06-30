package ai.economicdatasciences.dsa.memoize

class FactorialMemoiz {
  var cache: Map[Int, Int] = Map()

  // lookkkup function
  def lookup(num: Int): Int = cache.getOrElse(num, 0)

  def calcFactMemoiz(x: Int): Int = x match {
    case 0 => 1
    case 1 => 1
    case x if (lookup(x) > 0) => lookup(x)
    case _ => {
      val ans = x * calcFactMemoiz((x - 1))
      cache += x -> ans
      ans
    }
  }
}

object FactorialMemoizApp {
  def main(args: Array[String]): Unit = {
    val factMem = new FactorialMemoiz()
    println(factMem.calcFactMemoiz(3))
    println(factMem.calcFactMemoiz(5))
  }

}
