package ai.economicdatasciences.sandbox





// class PrimeContainer {
//
//   def squareRoot(x: Int): Double = {
//     scala.math.sqrt(x)
//   }
//
//   def genPrimesList(limit: Int): Stream[Int] = {
//     val numStream: Stream[Int] = 2 #:: Stream.from(3)
//     val primes = numStream.filter(x => {
//       val limitedNum = numStream.takeWhile(y => {
//         y <= squareRoot(x)
//       })
//       !limitedNum.exists(y => x % y == 0)
//     })
//
//     primes.take(limit)
//   }
// }
//
// object PrimeContainerApp {
//   def main(args: Array[String]): Unit = {
//     val pc = new PrimeContainer
//     println(pc.genPrimesList(15).toList.map(_ + ", ").mkString)
//   }
// }

// class FactorialMemoiz {
//   //* FACTORIAL WITH CACHING *//
//   var cache = Map[Int, Int]()
//
//   // lookup if exists in cache
//   def lookup(in: Int): Int = {
//     cache.getOrElse(in, 0)
//   }
//
//   // calculate factorial
//   def calcFactorial(x: Int): Int = {
//     x match {
//       case 0 => 1
//       case 1 => 1
//       case x2 => {
//         println("looking up...")
//         val lkup = lookup(x2)
//         lkup match {
//           case 0 => {
//             println("calculating...")
//             val ans = x * calcFactorial(x - 1)
//             cache += (x -> ans)
//             ans
//           }
//           case _ => lkup
//         }
//       }
//     }
//   }
// }
//
// object FactorialMemoizApp {
//   def main(args: Array[String]): Unit = {
//     val fm = new FactorialMemoiz
//     println(fm.calcFactorial(3))
//     println(fm.calcFactorial(5))
//   }
// }

// MULTIPLY BY TWO IN FUNCTIONAL STYLE
//
// import scala.annotation.tailrec
//
// object MultBy2App {
//   def main(args: Array[String]): Unit = {
//     val myArr = (0 to 10 by 2).toArray
//     multBy2(myArr).foreach(println)
//   }
//
//   def multBy2(arr: Array[Int]): Array[Int] = {
//     val out = Array[Int]()
//     @tailrec
//     def go(inarr: Array[Int], iter: Int, outarr: Array[Int]): Array[Int] = {
//       iter match {
//         case 0 => Array(inarr(iter)*2) ++ outarr
//         case _ => {
//           go(inarr, iter - 1, Array(inarr(iter)*2) ++ outarr)
//         }
//       }
//     }
//
//     go(arr, arr.length - 1, out)
//   }
//
// }
