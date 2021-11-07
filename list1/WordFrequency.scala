import scala.collection.mutable.Map
import scala.io.Source

class WordFrequency {
  val stopwords = Source.fromFile("stop_words_english.txt").getLines.toList
  val counts = Map[String, Map[String, Int]]()
  val allCounts = Map[String, Int]()
  val tfidfs = Map[String, Map[String, Double]]()

  def loadFile(filename: String) = {
    counts(filename) = Map[String, Int]()
    for (line <- Source.fromFile(filename).getLines) {
      countWords(line, filename)
    }
    calcTfidf()
  }

  def countWords(line: String, filename: String) = {
    for (
      word <- line
        .replaceAll("""[“”\p{Punct}]""", "")
        .split(" ")
        .map(_.toLowerCase)
    ) {
      if (word.length > 0 && !stopwords.contains(word)) {
        if (counts(filename).contains(word)) {
          counts(filename)(word) += 1
        } else {
          counts(filename)(word) = 1
        }

        if (allCounts.contains(word)) {
          allCounts(word) += 1
        } else {
          allCounts(word) = 1
        }
      }
    }
  }

  def tf(word: String, filename: String): Double = {
    (counts(filename)(word).toDouble / counts(filename).foldLeft(0)(_ + _._2))
  }

  def idf(word: String, filename: String): Double = {
    math.log(
      counts.size.toDouble / counts.count(_._2.contains(word))
    )
  }

  def tfidf(word: String, filename: String): Double = {
    tf(word, filename) * idf(word, filename)
  }

  def calcTfidf(): Unit = {
    for ((document, _) <- counts) {
      tfidfs(document) = Map[String, Double]()
      for ((word, _) <- counts(document)) {
        tfidfs(document)(word) = tfidf(word, document)
      }
    }
  }

  def printTopN(counts: Map[String, Int], n: Int) = {
    for ((k, v) <- counts.toSeq.sortBy(-_._2).take(n)) {
      println(s"$k -> $v")
    }
    println()
  }

  def printTopND(counts: Map[String, Double], n: Int) = {
    for ((k, v) <- counts.toSeq.sortBy(-_._2).take(n)) {
      println(s"$k -> $v")
    }
    println()
  }
}

object Task3 {
  def main(args: Array[String]): Unit = {
    var wordFreq = new WordFrequency()

    val book1 = "odyssey.txt"
    val book2 = "pride-and-prejudice.txt"

    wordFreq.loadFile(book1)
    wordFreq.loadFile(book2)

    wordFreq.printTopN(wordFreq.counts(book1), 10)
    wordFreq.printTopND(wordFreq.tfidfs(book1), 10)
    wordFreq.printTopN(wordFreq.counts(book2), 10)
    wordFreq.printTopND(wordFreq.tfidfs(book2), 10)

    wordFreq.printTopN(wordFreq.allCounts, 10)
  }
}
