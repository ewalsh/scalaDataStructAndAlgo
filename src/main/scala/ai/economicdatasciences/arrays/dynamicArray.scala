package ai.economicdatasciences.dsa.arrays

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class DynamicArray(mem_limit: Long) {
  // get runtime
  val rt = Runtime.getRuntime
  // initialize Array
  val dynArr = ArrayBuffer[Int]()

  // method to check current memory
  def currentMemUsage: Long = {
    rt.totalMemory - rt.freeMemory
  }

  // add random integer to array
  def addRandom: Unit = {
    val checkCurrent = currentMemUsage
    ((checkCurrent - mem_limit) > 0L) match {
      case true => {
        println("adding random integer to array")
        Random.nextInt +=: dynArr
        println(s"array is now length = ${dynArr.length}")
      }
      case false => {
        println("current memory limit is breached!")
      }
    }
  }

  // remove integer from front of array
  def removeHead: Unit = {
    dynArr.length match {
      case 0 => println("cannot remove anything from an array of length 0")
      case _ => {
        println("removing integer from start of array")
        dynArr -= dynArr.head
        println(s"array is now length = ${dynArr.length}")
      }
    }
  }
}

object DynamicArrayApp {
  def main(args: Array[String]): Unit = {
    val memLimit = args(0).toLong
    val continueExec = true
    val da = new DynamicArray(memLimit)
    while(continueExec){
      val checkCurrent = da.currentMemUsage
      ((checkCurrent - memLimit) > 0L) match {
        case true => da.addRandom
        case false => da.removeHead
      }
    }
  }
}
