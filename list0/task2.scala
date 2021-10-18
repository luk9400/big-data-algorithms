import scala.collection.mutable.Map
import scala.io.Source
import java.io._

object Task2 {
  def main(args: Array[String]): Unit = {
    val counts = Map[String, Int]()
    var stopwords = Array[String]()

    for (stopword <- Source.fromFile("stop_words_english.txt").getLines) {
      stopwords :+= stopword
    }


    for (line <- Source.fromFile("odyssey.txt").getLines) {
      for (word <- line.replaceAll("""[“”\p{Punct}]""", "").split(" ")) {
        if (word.length > 0 && !stopwords.contains(word)) {
          if (counts.contains(word)) {
            counts(word) += 1
          } else {
            counts(word) = 1
          }
        }
      }
    }

    val writer  = new PrintWriter(new File("counts.csv" ))
    for ((k, v) <- counts.toSeq.sortBy(-_._2).take(20)) {
      println(s"$k -> $v")
      writer.write(s"${v};${k}\n")
    }
    writer.close
  }
}
