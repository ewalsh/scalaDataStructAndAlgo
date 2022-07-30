package ai.economicdatasciences.dsa.queues

case class FQueue(out: List[Any], in: List[Any]) {
  def check(): Boolean = (out, in) match {
    case (Nil, x :: xs) => false
    case _ => true
  }

  require(check, "Didn't satisfy invariant")
}

object FunctQueueApp {
  def main(args: Array[String]): Unit = {
    val myQueue = insert(15, insert(10, insert(5, FQueue(Nil, Nil))))
    println(remove(myQueue))
  }

  def insert(data: Any, queue: FQueue): FQueue = {
    val newIn = data :: queue.in
    queue.out match {
      case Nil => FQueue(newIn.reverse, Nil)
      case _ => queue.copy(in = newIn)
    }
  }

  def remove(queue: FQueue): (Any, FQueue) = {
    queue.out match {
      case Nil => throw new IllegalArgumentException("Queue is empty!")
      case x :: Nil => (x, queue.copy(out = queue.in.reverse, Nil))
      case y :: ys => (y, queue.copy(out = ys))
    }
  }
}
