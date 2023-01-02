package WordCloud

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import scala.collection.{MapView, immutable, mutable}
import java.io.File
import scala.io.BufferedSource
import scala.io.Source.fromFile

object WordFileTest {

  def main(args: Array[String]): Unit = {
    val wordFile = new WordFile();
    val filenames = List("A Yorkshire Tragedy by William Shakespeare (Apocrypha).txt", "The Comedie of Errors by William Shakespeare.txt")
    val tfidt = wordFile.computeTFIDF(filenames)
    var words: Map[String, Int] = Map();
    var filesList: List[File] = List()
    var file: File = new File(filenames(0))
    for ((filename, bagOfWords)<- tfidt){
      val maps = bagOfWords.toSeq.sortBy(_._1)
      var file: File = new File(filename)
      val mostFreq = wordFile.getNMostFrequentWords(file)

      println(filename)
      println(mostFreq)
      println(maps)
    }

////    filesList =  file :: filesList
//    words = wordFile.getNMostFrequentWords(file, 10)
//    println("words")
//    println(words)
//
//    file= new File(filenames(1))
//    //    filesList =  file :: filesList
//    words = wordFile.getNMostFrequentWords(file, 10)
//    println("words")
//    println(words)
//    println(tfidt)


//    val wordPairs: Map[String, Int] = wordFile.getWordPairsCollection(filesList.head)
//    val fWordList: Map[String, Int] = wordFile.getNMostFrequentWords(filesList.head, 10)
//    wordFile.computeTFMeasureForOneFile(fWordList)
//    println(fWordList)
//    val fWordString: String = fWordList.toString()
//    val fileToSave = new File("most_frequent_words.txt")
//    val bw = new BufferedWriter(new FileWriter(fileToSave))
//    bw.write(fWordString)
//    bw.close()

}}