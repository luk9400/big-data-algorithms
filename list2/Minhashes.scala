// task 3
import scala.collection.mutable.Set
import scala.io.Source
import scala.util.Random
import scala.math.pow

object Shingles {
  val RNG = pow(2, 32).toInt

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

  def generateAB(n: Int): Array[Tuple2[Int, Int]] = {
    var arr = Array[Tuple2[Int, Int]]()

    for (i <- 0 until n) {
      arr = arr ++ Array((Random.nextInt(RNG), Random.nextInt(RNG)))
    }

    return arr
  }

  def minhash(set: Set[String], ab: Tuple2[Int, Int]): Int = {
    set
      .map(x => (ab._1 * x.hashCode() + ab._2) % RNG)
      .min
  }

  def signature(
      set: Set[String],
      hashes: Array[Tuple2[Int, Int]]
  ): Array[Int] = {
    hashes.map(x => minhash(set, x))
  }

  def compareSignatures(sig1: Array[Int], sig2: Array[Int]): Double = {
    var counter = 0
    for (i <- 0 until sig1.length) {
      if (sig1(i) == sig2(i)) {
        counter += 1
      }
    }

    return counter.toDouble / sig1.length
  }

  def main(args: Array[String]): Unit = {
    val k = 3
    val oddyseyShingles = getKShignles(k, "odyssey.txt")
    val prideShingles = getKShignles(k, "pride-and-prejudice.txt")

    val hashes = generateAB(100)

    val oddyseySignature = signature(oddyseyShingles, hashes)
    val prideSignature = signature(prideShingles, hashes)

    println(compareSignatures(oddyseySignature, prideSignature))
    println(jaccard(oddyseyShingles, prideShingles))
  }
}
