import objects.animals._
import filters._

import java.util


object CountingBloomFilterTest {

  def main(args: Array[String]): Unit = {

    val stream = args.map(makeAnimalObject).to(LazyList)
    val rabbit = stream(0)
    val sloth = stream(1)
    val rat = stream(2)
    val wolf = new Wolf()
    val countingFilter = new CountingBloomFilter(stream)
    countingFilter.apply()

    println("Elements in stream: ")
    countingFilter.printElementsInStream()
    println("\nWolf is in stream:" + countingFilter.check(wolf))
    println("\nRabbit is in stream:" + countingFilter.check(rabbit))
    println("\nSloth is in stream:" + countingFilter.check(sloth))
    println("\nRat is in stream:" + countingFilter.check(rat))

    println(" \nAdding an element.")
    countingFilter.add(wolf)
    println("\nWolf is in stream:" + countingFilter.check(wolf))
    println("Removing an element rabbit")
    countingFilter.remove(rabbit)
    println("\nRabbit is in stream:" + countingFilter.check(rabbit))
    println("Adding 140 wolfs and 25 rats")
    for( _ <- 0 until 140)
    {
      countingFilter.add(wolf)
    }
    for( _ <- 0 until 25)
      {
        countingFilter.add(rat)
      }
    println("Wolf count (should be 141): "+ countingFilter.count(wolf))
    println("Rat count (should be 26): "+ countingFilter.count(rat))
    println("Rabbit count (should be 0): "+ countingFilter.count(rabbit))
    println("Sloth count (should be 1): "+ countingFilter.count(sloth))



  }


  def makeAnimalObject(name: String): Object = {
    val animalsMap = Map("rabbit" -> new Rabbit(), "rat" -> new Rat(), "sloth" -> new Sloth(), "wolf" -> new Wolf());

    try {
      animalsMap(name)
    }
    catch {
      case _: util.NoSuchElementException => throw new IllegalArgumentException(name + " is not an available animal name.")
    }

  }
}