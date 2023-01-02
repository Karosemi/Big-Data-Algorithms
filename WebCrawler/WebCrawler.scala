package WebCrawler

import org.jsoup.Jsoup

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.ListBuffer

object WebCrawler {

  val linksMap: collection.mutable.Map[String, Int] = collection.mutable.Map()


  def main(args: Array[String]) = {
  val urlList = List("https://towardsdatascience.com/", "https://medium.com/", "https://datascience.codata.org/",
                     "https://blog.kaggle.com/", "https://www.kaggle.com/")
//    val linksList: ListBuffer[ListBuffer[String]] = collection.mutable.ListBuffer()
    val oneUrllinksMap: collection.mutable.Map[String, ListBuffer[String]] = collection.mutable.Map()
    for (url <- urlList){
//      linksList.addOne(getLinksOfPage(url): ListBuffer[String])
      oneUrllinksMap += (url -> getLinksOfPage(url))
    }
//    val newLinks = getLinksOfPage(urlList(0))
//    newLinks.foreach(println )
//    println(oneUrllinksMap)
    println("dd")
    val oneLine: ListBuffer[Double] = collection.mutable.ListBuffer()
    type Row = Vector[Int]
    type Matrix = Vector[Row]
    val allRows: ListBuffer[Vector[Double]] = ListBuffer()
    println(linksMap)
    val rowLength = linksMap.size
//    for ((url_0, _) <- linksMap){
//      println(url_0)
//      val links =  getLinksOfPage(url_0)
//      println(links)
//      val prob = 1.0/links.length
//      for ((url, _) <- linksMap){
//        if (links.contains(url)){
//          oneLine.addOne(prob)
//        }
//        else{
//          oneLine.addOne(0.0)
//        }
//      }
//      val oneRow: Vector[Double] = oneLine.toVector
//      allRows.addOne(oneRow)


//    }
//    val allRowsVector = allRows.toVector
//    println("dksks")
//    println(allRowsVector.size)
//    println(allRowsVector)

//    println(linksMap(newLinks(0)))
    //    val newLinks = linksExtractor(url).zipWithIndex.map(_.swap).toMap.groupMap(_._2)(_._1)
//    linksMap ++ newLinks.map{case (a:String,b: List[Int])=> (a, b.head)}

}


  def getLinksOfPage(url:String): ListBuffer[String] ={
    var newKey = 1
    if (linksMap.nonEmpty){
      newKey = linksMap.values.toList.max + 1}
    if (! linksMap.contains(url)){
    linksMap += (url -> newKey)}
    else {
    newKey += 1}
    val newLinks = linksExtractor(url)
    for (oneLink <- newLinks){
      linksMap += (oneLink -> newKey)
      newKey += 1
    }
    newLinks
  }
def linksExtractor(url: String): collection.mutable.ListBuffer[String] ={
  val connection = Jsoup.connect(url)
//  Thread.sleep(1000)
  val conget = connection.get()
  val selected = conget.select("a[href]").unwrap().asScala
  val newLinks: collection.mutable.ListBuffer[String] = collection.mutable.ListBuffer()
  for (one <- selected){
    val partCut = one.toString.substring(9)
    var links = partCut.substring(0, partCut.indexOf('"'))
    if (!(links.equals(""))){
      if (links.substring(0,1).equals("h")){
        val revString = reverse(links)
        val lastSlash =  links.length - revString.indexOf('/')
        links = links.substring(0, lastSlash)
        newLinks.addOne(links)
      }}}
  newLinks.distinct
}
  def reverse(s: String) = ("" /: s)(_.+:(_))
}
