package ai.economicdatasciences.queues

case class Fifo(out: List[Int], in: List[Int]) {
  def check(): Boolean = (out, in) match {
    case (Nil, x :: xs) => false
    case _ => true
  }
  require(check, "Invariant Failed - out.em")
}

object QueueOps {
  // def apply(in: Int): Unit = {
  //   println(s"$in")
  // }
  def push(e: Int, queue: Fifo): Fifo = {
    val newIn = e :: queue.in
    queue.out match {
      case Nil => Fifo(newIn.reverse, Nil)
      case _ => queue.copy(in = newIn)
    }
  }

  def pop(queue: Fifo): (Int, Fifo) = {
    queue.out match {
      case Nil => throw new IllegalArgumentException("Empty queue")
      case x :: Nil => (x, queue.copy(out = queue.in.reverse, Nil))
      case y :: ys => (y, queue.copy(out = ys))
    }
  }
}
