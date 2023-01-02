import scala.util.control.Breaks._

object GreatestDivisor {
  def main(args: Array[String]) {

    args.foreach(arg => getDivisor(arg))
  }


  private def getDivisor(stringNumber: String): Unit = {
    val number = tryConvertToInt(stringNumber)
    if (number == 0){
      println(stringNumber + ": the conversion is not possible")
    }
    breakable {
      for (i <- 2 to number) {
        val divisor = number / i;
        if (divisor * i == number) {
          println(number +":" + divisor);
          break

        }
      }
    }
  }

  private def tryConvertToInt(value: String): Int ={
    try {
      value.toInt
    } catch {
      case ex: NumberFormatException =>{
       0}
    }
  }
}