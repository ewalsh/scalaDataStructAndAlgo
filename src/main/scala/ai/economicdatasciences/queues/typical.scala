package ai.economicdatasciences.dsa.queues

import java.time.Instant
import java.time.Duration

class MyQueue(maxSize: Int) {
  // private
  var queueBox: Array[Any] = new Array[Any](maxSize)
  // private
  var front: Int = 0
  // private
  var rear: Int = -1
  // private
  var numOfItems: Int = 0

  def insert(data: Any): Unit = {
    if (rear == (maxSize - 1)) {
      rear = -1
    }

    rear += 1
    queueBox(rear) = data
    numOfItems += 1
  }

  def remove(): Any = {
    val tempData: Any = queueBox(front)
    front += 1

    if (front == maxSize) {
      front = 0
    }

    numOfItems -= 1
    tempData
  }

  def peekFront(): Any = {
    queueBox(front)
  }

  def isEmpty(): Boolean = {
    numOfItems == 0
  }

  def isFull(): Boolean = {
    numOfItems == maxSize
  }

  def slidingAvg(size: Int, step: Int): Option[List[Any]] = {
    // only run for int class for now
    val classCheck = queueBox.map(_.toString.toInt.getClass.toString).exists(_ != "int")
    classCheck match {
      case true => {
        println("can only use this function if array is convertable to ints")
        None
      }
      case false => {
        Some(queueBox.sliding(size, step).toList.map(x => {
          val y = x.map(_.toString.toInt)
          y.sum / y.length
        }))
      }
    }
  }
}

object QueueApp {
  def main(args: Array[String]): Unit = {
    val myQueue = new MyQueue(10000)

    var perfInput = 0
    for(i <- Range(0, 10000)){
      val start = Instant.now()
      myQueue.insert(perfInput)
      val end = Instant.now()
      perfInput = Duration.between(start, end).toNanos.toInt
    }

    // myQueue.insert(5)
    // myQueue.insert(10)
    // myQueue.insert(15)
    //
    while (!myQueue.isEmpty()) {
      println(myQueue.remove())
    }
  }
}
