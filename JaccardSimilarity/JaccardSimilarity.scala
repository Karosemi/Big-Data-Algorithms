package JaccardSimilarity

import java.io.File
import scala.io.BufferedSource
import scala.io.Source.fromFile

object JaccardSimilarity {
  val stopWordsFilename = "stopwords.txt"
  val stopWordsFile = new File(stopWordsFilename)
  val stopWordsSource: BufferedSource = fromFile(stopWordsFile)
  val stopWords: String = stopWordsSource.getLines().mkString
    stopWordsSource.close()


  def main(args: Array[String]): Unit = {
    val filenames = List("A Yorkshire Tragedy by William Shakespeare (Apocrypha).txt", "The Comedie of Errors by William Shakespeare.txt",
      "The Passionate Pilgrim by William Shakespeare.txt", "The Winters Tale by William Shakespeare.txt")
    var filesMap = scala.collection.mutable.Map[String, Double]()

    var file1: File = new File(filenames.head)
    var file2: File = new File(filenames.head)
    var wordsList1: List[String]= List()
    var wordsList2: List[String]= List()
    val ResultsMap = scala.collection.mutable.Map[Int, collection.mutable.Map[String, Double]]()
    for (k <- 5 until 6) {
      for (i <- filenames.indices) {
        for (j <- filenames.indices) {
          if (i < j) {
            file1 = new File(filenames(i))
            file2 = new File(filenames(j))
            wordsList1 = readWords(file1)
            wordsList2 = readWords(file2)
            filesMap += (((file1.getName) + " & " + (file2.getName)) ->
              jaccardSimilarityForTwoFiles(createKShingles(wordsList1, k),
                createKShingles(wordsList2, k)))
          }

        }
      }
      ResultsMap += (k -> filesMap)
    }
    println(ResultsMap)
  }
  // minhashing



  def getWordsForManyFiles(fileList: List[File]): Map[String, Map[String, Int]]={
    var outputMap: Map[String, Map[String, Int]] = Map()
    for (i <- fileList.indices){
      outputMap = outputMap + (fileList(i).getName -> getWordPairsCollection((fileList(i))))
    }
    outputMap
  }

  def getWordPairsCollection(file: File): Map[String, Int] = {
    val wordList = readWords(file)
    wordList.groupBy(identity).mapValues(_.size).toMap
  }

  def getWordPairsCollectionForList(wordList: List[String]): Map[String, Int] = {
    wordList.groupBy(identity).mapValues(_.size).toMap
  }

  def readWords(file: File): List[String] = {
    val fileSource: BufferedSource = fromFile(file)
    val linesList = fileSource.getLines().toList.
      mkString(" ").replaceAll("[^a-zA-z ]", "").toLowerCase().split("\\s+")
      .filter(word => !stopWords.contains(word))
      .toList
    fileSource.close()
    linesList
  }
  def jaccardSimilarityForTwoFiles(file1: Set[List[String]], file2: Set[List[String]]): Double = {

    file1.map(ls => ls.mkString("\\s")).intersect(file2.map(ls => ls.mkString("\\s"))).size.toDouble /
      file1.map(ls => ls.mkString("\\s")).union( file2.map(ls => ls.mkString("\\s"))).size.toDouble
  }

  def createKShingles(wordsFromBook: List[String], k: Int): Set[List[String]] = {
    //sliding function
    var totalKShingles: List[List[String]] = List(List())
    for (i <- wordsFromBook.indices) {
      var kShingles: List[String] = List()
      for (k <- 0 until k) {
        if (!((i + k - 1) >= wordsFromBook.size - 1)) {
          kShingles = kShingles :+ wordsFromBook(i + k)
        }
      }
      if (kShingles.size == k) {
        totalKShingles = totalKShingles :+ kShingles
      }
    }
//    println(totalKShingles.toSet)
    totalKShingles.toSet
  }
}
