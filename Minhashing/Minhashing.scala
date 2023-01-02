package Minhashing
import java.io.File
import scala.io.BufferedSource
import scala.io.Source.fromFile
import scala.util.Random

object Minhashing {

  val stopWordsFilename = "stopwords.txt"
  val stopWordsFile = new File(stopWordsFilename)
  val stopWordsSource: BufferedSource = fromFile(stopWordsFile)
  val stopWords: String = stopWordsSource.getLines().mkString
  stopWordsSource.close()


  def main(args: Array[String]): Unit = {
    val filenames = List("A Yorkshire Tragedy by William Shakespeare (Apocrypha).txt", "The Comedie of Errors by William Shakespeare.txt",
      "The Passionate Pilgrim by William Shakespeare.txt", "The Winters Tale by William Shakespeare.txt")

    val setKShingles = scala.collection.mutable.Map[String, Array[Set[String]]]()
    val filesMap = scala.collection.mutable.Map[Int, collection.mutable.Map[String, Double]]()
    val ResultsMap = scala.collection.mutable.Map[Int, scala.collection.mutable.Map[Int, collection.mutable.Map[String, Double]]]()

    val hashFunctions = Set(10, 100, 250, 500)
//    val ResultsMap = scala.collection.mutable.Map[Int, collection.mutable.Map[String, Double]]()
    for (hashingFunction <- hashFunctions) {
      for (k <- 5 until 6) {
        var hashingTablesSet = Array[Array[Int]]()
        for (i <- filenames.indices) {
          val file: File = new File(filenames(i))
          val kShringles = createKShingles(readWords(file), k)

          hashingTablesSet = createSetOfHashingTables(kShringles.length, hashingFunction)
          setKShingles += (file.getName -> kShringles)
        }
        val resultsMap = scala.collection.mutable.Map[String, Double]()
        for (i <- filenames.indices) {
          for (j <- filenames.indices) {
            if (i < j) {
              val minhashSets = calculateMinhashSignatures(setKShingles.toMap, hashingTablesSet, (i) => (3 * i % 5))
              val file1 = new File(filenames(i))
              val file2 = new File(filenames(j))
              resultsMap += (((file1.getName) + " & " + (file2.getName)) -> jaccardSimilarityGivenSignature(minhashSets, (file1.getName), (file2.getName)))
            }
          }
        }
        filesMap += (k -> resultsMap)
      }
      ResultsMap += (hashingFunction -> filesMap)
    }
    println(ResultsMap)
  }
  // minhashing

  def calculateMinhashSignatures(shingleSets: Map[String, Array[Set[String]]], hashingTablesSet: Array[Array[Int]], hashFunctions: (Int) => Int): Map[String, Set[Int]] = {
    val minhashSet = collection.mutable.Map[String, Set[Int]]()
    shingleSets
      .foreach(documentShingle => {
        val minhash: collection.mutable.Set[Int] = collection.mutable.Set[Int]()
        for (ht <- hashingTablesSet.indices) {
          val valueAfterHashing: Int = hashingTablesSet(ht)(hashFunctions(documentShingle._2.length))
          minhash.add(valueAfterHashing)
        }
        minhashSet.addOne((documentShingle._1 -> minhash.toSet))
      })
    minhashSet.toMap
  }



  def jaccardSimilarityGivenSignature(minhashSets: Map[String, Set[Int]], key1: String, key2: String): Double = {
    val signature1 = minhashSets(key1)
    val signature2 = minhashSets(key2)
    signature1.intersect(signature2).size.toDouble / signature1.union(signature2).size.toDouble
  }

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
  def createSetOfHashingTables(arrayLength: Int, hashingFunctions: Int): Array[Array[Int]] = {
    val hashingTablesSet = collection.mutable.Set[Array[Int]]()
    for (_ <- 0 until hashingFunctions) {
      hashingTablesSet += createHashingTable(arrayLength)
    }
    hashingTablesSet.toArray
  }

  def createHashingTable(arrayLength: Int): Array[Int] = {
    Random.shuffle(Array.range(0, arrayLength - 1)).toArray
  }


  def createKShingles(wordsFromBook: List[String], k: Int): Array[Set[String]] = {
    var totalKShingles: Array[Set[String]] = Array(Set())
    for (i <- wordsFromBook.indices) {
      var kShingles: Set[String] = Set()
      for (k <- 0 until k) {
        if (!((i + k - 1) >= wordsFromBook.size - 1)) {
          kShingles += wordsFromBook(i + k)
        }
      }
      if (kShingles.size == k) {
        totalKShingles = totalKShingles :+ kShingles
      }
    }
    totalKShingles
  }

  }

