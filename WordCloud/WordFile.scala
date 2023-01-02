package WordCloud
import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.{MapView, mutable}
import scala.collection.mutable.Map
import scala.io.BufferedSource
import scala.io.Source.fromFile
import scala.math.Numeric.BigDecimalAsIfIntegral.mkNumericOps

class WordFile {
  val stopWordsFilename = "stopwords.txt"
  val stopWordsFile = new File(stopWordsFilename)
  val stopWordsSource: BufferedSource = fromFile(stopWordsFile)
  val stopWords: String = stopWordsSource.getLines().mkString
  stopWordsSource.close()



  def getWordsForManyFiles(fileList: List[File]): mutable.Map[String, mutable.Map[String, Int]]={
    var outputMap: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map()
    for (i <- fileList.indices){
      outputMap = outputMap + (fileList(i).getName -> getWordPairsCollection((fileList(i))))
    }
    outputMap
  }

  def getNMostFrequentWordsForManyFiles(fileList: List[File]): mutable.Map[String, mutable.Map[String, Int]]={
    var outputMap: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map()
    for (i <- fileList.indices){
      outputMap = outputMap + (fileList(i).getName -> getNMostFrequentWords(fileList(i)))
    }
    outputMap
  }


//  def preprocessFiles(filenames: List[String]) : mutable.Map[String, mutable.Map[String, Int]]={
//    var filesMap: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map()
//    var file: File = new File(filenames.head)
//    for (i <- filenames.indices) {
////      filesList =  file :: filesList
//      file = new File(filenames(i))
//      filesMap = filesMap + (file.getName -> getWordPairsCollection(file))
//      //      println(filenames(i))
//      //      println(filesList)
//    }
//    filesMap
//  }



  def computeTFIDF(filenames: List[String]) : mutable.Map[String, mutable.Map[String, Double]]={
    val filesMap: mutable.Map[String, mutable.Map[String, Double]] = mutable.Map()
    //biore wszystkie slowa aby policzyc df - czyli ilosc dokumentow w ktorym dane slowo wystapilo raz
    val allWords: List[String] = readWordsFromFiles(filenames).distinct
    val allBagOfWords: mutable.Map[String, mutable.Map[String, Int]] = getBagsOfWordsForFiles(filenames)
    //tutaj policze sobie ilosc dokumentow w ktorym wystapilo dane slowo
    val bagOfOccurness: mutable.Map[String, Int] = mutable.Map()
    for (oneWord <- allWords){
      if (!(bagOfOccurness.contains(oneWord))){
        bagOfOccurness += (oneWord -> 0)
      }
      for ((_, oneBag) <- allBagOfWords){
        if (oneBag.contains(oneWord)){
          bagOfOccurness(oneWord) += 1
        }
      }
    }
    val D = filenames.length
    var file: File = new File(filenames.head)
    var wordsN =0;

    for (i <- filenames.indices) {

      file = new File(filenames(i))
      val bagOfWords = allBagOfWords(file.getName)
      val newBagOfWords: mutable.Map[String, Double] = mutable.Map()
      //ilosc wszystkich wystapien slow w dokumencie
      wordsN = bagOfWords.foldLeft(0)(_ + _._2)
      for ((word, freq) <- bagOfWords) {

        // liczba dokumentow zawierajaca przynajmniej jedno wystapienie danego slowa
        val idf = math.log((D.toDouble+1) / (bagOfOccurness(word)+1))
        val tf = freq.toDouble / wordsN
        val tfidf = tf * idf
        newBagOfWords += (word -> tfidf)
      }
      filesMap(file.getName) = newBagOfWords

    }
    filesMap
  }




  def saveMostFrequentWords(file: File, wordsNum: Int, filename: String="most_frequent_words.txt"): Unit={
    val mostFrequentWords = getNMostFrequentWords(file, wordsNum)
    val fWordString: String = mostFrequentWords.toString()
    val fileToSave = new File(filename)
    val bw = new BufferedWriter(new FileWriter(fileToSave))
    bw.write(fWordString)
    bw.close()

  }

  def getNMostFrequentWords(file: File, wordsNum: Int=10): mutable.Map[String, Int] ={
    var wordPairs = getWordPairsCollection(file)
    var word = wordPairs.maxBy(_._2)._1
    var value = wordPairs.maxBy(_._2)._2
    var mostFrequentWordsMap: mutable.Map[String, Int] = mutable.Map()
    for( _ <- 0 until wordsNum){
      mostFrequentWordsMap = mostFrequentWordsMap + (word -> value)
      wordPairs = wordPairs.-(word)
      word = wordPairs.maxBy(_._2)._1
      value = wordPairs.maxBy(_._2)._2

    }
    mostFrequentWordsMap
  }

  def readFiles(filenames: List[String]): Unit ={
    var filesList: List[File] = List()
    for (filename <- filenames){
      var file: File = new File(filename)
      filesList = filesList :+ file
    }
  }

  def readWordsFromFiles(filenames: List[String]): List[String] ={

    var allWords: List[String]= List()
    for (i <- filenames.indices) {
      val file = new File(filenames(i))
      allWords = allWords ++ readWords(file)
    }
    allWords
  }


  def getBagsOfWordsForFiles(filenames: List[String]): mutable.Map[String, mutable.Map[String, Int]]={
    val bagsOfWords : mutable.Map[String, mutable.Map[String, Int]] = mutable.Map()
    for (i <- filenames.indices) {
      val file = new File(filenames(i))
      bagsOfWords +=  (file.getName -> getWordPairsCollectionForList(readWords(file)))


    }
    bagsOfWords
  }
  def getWordPairsCollectionForList(wordList: List[String]): mutable.Map[String, Int] = {
    mutable.Map() ++ wordList.groupBy(identity).mapValues(_.size).toMap
  }



  def getWordPairsCollection(file: File): mutable.Map[String, Int] = {
    val wordList = readWords(file)
    mutable.Map() ++ wordList.groupBy(identity).mapValues(_.size).toMap
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
}
