// task 2
import scala.collection.mutable.Set
import scala.io.Source

object Shingles {
  def getKShignles(k: Int, book: String): Set[String] = {
    val tokens = Source
      .fromFile(book)
      .getLines()
      .reduce(_ ++ " " ++ _)
      .replaceAll("""[“”\p{Punct}]""", "")
      .split(" ")
      .map(_.toLowerCase)

    val set = Set[String]()

    for (i <- 0 until tokens.length - k + 1) {
      set += tokens.slice(i, i + k).mkString(" ")
    }

    return set
  }

  def jaccard(set1: Set[String], set2: Set[String]): Double = {
    if (set1.size > 0 || set2.size > 0) {
      return set1.intersect(set2).size.toDouble / set1.union(set2).size
    } else {
      return 1
    }
  }

  def main(args: Array[String]): Unit = {
    val k = 13
    val oddyseyShingles = getKShignles(k, "odyssey.txt")
    val prideShingles = getKShignles(k, "pride-and-prejudice.txt")

    println(jaccard(oddyseyShingles, prideShingles))
  }
}
