package filters

import scala.collection.mutable.ListBuffer

class CountingBloomFilter(var elementStream: LazyList[Object]) {
  val bloomFilterSize = 11
  var countingBloomFilterArray: Array[Int] = new Array[Int](bloomFilterSize)

  init()

  private def init(): Unit = {
    countingBloomFilterArray.foreach(_ => 0)
  }

  def printElementsInStream(): Unit = {
    elementStream.foreach(i => print(i.toString + ", "))
  }

  def printCountingBloomFilter(): Unit = {
    countingBloomFilterArray.foreach(i => print(i + ", "))
  }

  def apply(): Unit = {
    elementStream.foreach(e => add(e))
  }

  private def hash(element: Object): ListBuffer[Int] = {
    val keys: ListBuffer[Int] = ListBuffer.empty

    val firstKey: Int = math.abs(element.hashCode() % bloomFilterSize)
    val secondKey: Int = math.abs((2 * element.hashCode()) % bloomFilterSize)
    val thirdKey: Int = math.abs((3 * element.hashCode()) % bloomFilterSize)


    keys += (firstKey)
    keys += (secondKey)
    keys += (thirdKey)

    keys
  }


  def add(element: Object): Unit = {
    val keys = hash(element)
    var key = 0
    for (i <- keys.indices) {
      key = keys(i) % bloomFilterSize
      countingBloomFilterArray(key) += 1
    }
  }


  def remove(element: Object): Unit = {
    val keys = hash(element)
    var key = 0
    while (check(element)) {
      for (i <- keys.indices) {
        key = keys(i) % bloomFilterSize
        countingBloomFilterArray(key) -= 1
      }
    }

  }


  def check(element: Object): Boolean = {
    var found = true
    val keys = hash(element)
    var key = 0
    for (i <- keys.indices) {
      key = keys(i) % bloomFilterSize
      if (!(countingBloomFilterArray(key) > 0)) {
        found = false
      }
    }
    found
  }

  def count(element: Object): Int = {
    val keys = hash(element)
    val values = new ListBuffer[Int]
    for (i <- keys.indices) {
      values += (countingBloomFilterArray(keys(i)))
    }
    values.toList.min
  }

}