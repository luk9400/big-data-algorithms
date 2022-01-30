import java.io._
import scala.io.Source.fromFile
import scala.io.Source.fromURL
import scala.collection.mutable.Map
import scala.collection.mutable.Set

object Crawler {
  val baseURL = "https://en.wikipedia.org/wiki/"
  val links = Map[String, Set[String]]()
  val re = "href=\"/wiki/([\\w]+)\"".r

  def crawl(page: String, iterations: Int): Boolean = {
    val pageStr = fromURL(baseURL + page).mkString
    links(page) = Set[String]()

    re.findAllMatchIn(pageStr).foreach { m =>
      links(page).add(m.group(1))
    }

    if (iterations > 0) {
      if (links(page).size > 0) {
        val n = util.Random.nextInt(links(page).size)

        crawl(links(page).iterator.drop(n).next, iterations - 1)
      } else {
        return false
      }
    } else {
      return true
    }
  }

  def inNeighbours(
      node: String,
      links: Map[String, Set[String]] = links
  ): Array[String] = {
    links
      .map(entry => if (entry._2.contains(node)) entry._1 else "")
      .toArray
      .filter(item => item != "")
  }

  def toFile(links: Map[String, Set[String]]): Unit = {
    val writer = new PrintWriter(new File(s"crawled_pages_${links.size}.txt"))
    links
      .foldLeft(Array[Tuple2[String, String]]())((acc, item) =>
        acc ++ item._2.map(elem => (item._1, elem))
      )
      .foreach(item => writer.write(s"${item._1} ${item._2}\n"))
    writer.close()
  }

  def loadFile(filename: String): Map[String, Set[String]] = {
    val links = Map[String, Set[String]]()

    fromFile(filename)
      .getLines()
      .foreach(line => {
        if (line.split("\\s+").length == 1) {
          links(line.split("\\s+")(0)) = Set()
        } else {
          val Array(u, v) = line.split("\\s+")
          if (links.contains(u)) {
            links(u).add(v)
          } else {
            links(u) = Set(v)
          }
        }
      })

    return links.mapValuesInPlace((k, v) =>
      v.filter(elem => links.contains(elem))
    )
  }

  def pageRank(
      links: Map[String, Set[String]],
      iterations: Int = 100,
      beta: Double = 0.85
  ): Map[String, Double] = {

    val ranks =
      links.foldLeft(Map[String, Double]())((acc, item) =>
        acc.addOne(item._1 -> 1.0 / links.size)
      )

    for (i <- 0 until iterations) {
      for (node <- links.keys) {
        val pagerankSum =
          inNeighbours(node, links).map(k => ranks(k) / links(k).size).sum

        ranks(node) = (1 - beta) / links.size + beta * pagerankSum
      }
    }

    return ranks
  }

  def avgNumOfLinksPerPage(links: Map[String, Set[String]]): Int = {
    links.map(item => item._2.size).sum / links.size
  }

  def main(args: Array[String]): Unit = {
    // CRAWL MODE
    // crawl("PageRank", 10)
    // toFile(links)

    // PAGERANK MODE
    val graph = loadFile("spider_trap.txt")
    println(graph mkString "\n")
    val ranks = pageRank(graph)
    println("=== RESULTS ===:")
    println(ranks.toSeq.sortBy(-_._2) mkString "\n")
    println("=== PAGERANK SUM ===:")
    println(ranks.values.sum)
  }
}
