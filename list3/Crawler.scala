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

    // println(s"\n ${baseURL + page}")
    // println(links(page) mkString "\n")

    if (iterations > 0) {
      if (links(page).size > 0) {
        val n = util.Random.nextInt(links(page).size)

        // TODO: add trap prevention
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
    links.foldLeft(Array[String]())((acc, entry) =>
      if (entry._2.contains(node)) acc :+ entry._1 else acc
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

    println(ranks)

    for (i <- 1 to iterations) {
      for (node <- links.keys) {
        val pagerankSum =
          inNeighbours(node, links).map(k => ranks(k) / links(k).size).sum

        ranks(node) = beta / links.size + (1 - beta) * pagerankSum
      }
    }

    return ranks

  }

  def main(args: Array[String]): Unit = {
    crawl("PageRank", 10)
    val ranks = pageRank(links)
    println(ranks)
    println(ranks.values.sum)
  }
}
