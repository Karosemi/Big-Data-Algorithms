package primeNumber
class PrimeNumber(val n: Int){

  def number(m: Int): String={
    val primes  = calculatePrimeNumbers();
    val primes_number = primes.length;
    if (m<primes_number)
      primes(m).toString
    else
      "out of range" : String
  }

  private def calculatePrimeNumbers(): IndexedSeq[Int] = {
    (2 to n).filter(isPrime)
  }

  private def isPrime(i : Int): Boolean = {
    if (i == 2)
      true
    else
      !(2 until i).exists(n => i % n ==0)
  }

}