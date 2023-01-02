package GraphAnalysis
import java.io.File
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile
import scala.io.{BufferedSource, Source}


object GraphAnalysis {
  val listIn = new ListBuffer[Char]()
  val listOut = new ListBuffer[Char]()


  def main(args: Array[String]) = {
    val graphFile = new File("SimpleGraph.txt")
    val MapReducedGraphList = getMapReducedGraph(graphFile)
      for ((node, (in, out))<- MapReducedGraphList){
        println("node: " + node + ", inDeg: "+in.toString + ", outDeg: "+out.toString) }
    // second graph
//    val stanfordGraphFile = new File("web-Stanford.txt")
//    val MapReducedStanfordGraphList = getMapReducedGraph(stanfordGraphFile)
//    for ((node, (in, out))<- MapReducedStanfordGraphList){
//      println("node: " + node + ", inDeg: "+in.toString + ", outDeg: "+out.toString) }

  }


 def getMapReducedGraph(file: File): List[Tuple2[Char,  (Int, Int)]] ={
   val fileSource: BufferedSource = fromFile(file)
   val graphMapReduceTask = fileSource.getLines().map(x => (x(0), x(2))).map { case (a, b) => List((a, (0, 1)), (b, (1, 0))) }
     .reduce[ List[Tuple2[Char, Tuple2[Int, Int]]] ] { case (a:List[Tuple2[Char, Tuple2[Int, Int]]], b: List[Tuple2[Char, Tuple2[Int, Int]]]) => a++ b }
     .groupBy(_._1) map[Tuple2[Char,  (Int, Int)]]{ case (_: Char, v: List[Tuple2[Char, (Int, Int)]]) => v
     .reduce[(Char, (Int, Int))] {case (x: (Char, (Int, Int)), y: (Char, (Int, Int))) =>  (x._1 ,(x._2._1+y._2._1 : Int,x._2._2+y._2._2 :Int ))}}
   graphMapReduceTask.toList
 }

}
