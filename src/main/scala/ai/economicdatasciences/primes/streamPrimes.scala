package ai.economicdatasciences.dsa.primes

object PrimeNumbersApp {
  def main(args: Array[String]): Unit = {
    println(primes.take(15).toList)
  }

  val primes: Stream[Int] = Stream(2) ++ Stream
    .from(3)
    .filter(x => {
      val sqrtOfPrimes = primes.takeWhile(y => y <= math.sqrt(x))
      !sqrtOfPrimes.exists(y => x % y == 0)
    })

}
