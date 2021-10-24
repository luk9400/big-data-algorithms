import scala.collection.mutable.Map
import scala.io.Source
import java.io._
import scala.io.StdIn.readLine

class WordCloud {
  val stopwords = Source.fromFile("stop_words_english.txt").getLines.toList
  val counts = Map[String, Int]()

  def loadFile(filename: String) = {
    for (line <- Source.fromFile(filename).getLines) {
      countWords(line)
    }
  }

  def countWords(line: String) = {
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

  def generateCsv(filename: String) = {
    val writer  = new PrintWriter(new File(s"${filename}.csv" ))
    for ((k, v) <- counts.toSeq.sortBy(-_._2)) {
      writer.write(s"${v};${k}\n")
    }
    writer.close
  }

  def printTopN(n: Int) = {
    for ((k, v) <- counts.toSeq.sortBy(-_._2).take(n)) {
      println(s"$k -> $v")
    }
  }
}

object Task3 {
  def main(args: Array[String]): Unit = {
    var wordCloud = new WordCloud()
    var command = readLine("Type \"help\" to get some help\n> ")

    while (command != "exit") {
      try {
        command match {
          case "new" => wordCloud = new WordCloud()
          case "file" => {
            val filename = readLine("Filename: ")
            wordCloud.loadFile(filename)
          }
          case "line" => {
            val line = readLine("Line: ")
            wordCloud.countWords(line)
          }
          case "csv" => {
            val filename = readLine("Filename: ")
            wordCloud.generateCsv(filename)
          }
          case "print" => {
            val n = readLine("Number of words: ")
            wordCloud.printTopN(n.toInt)
          }
          case "help" => {
            println("""
            new - restarts the generator
            file - parses file
            line - parses line
            csv - generates csv
            print - prints the most common words
            help - it helps
            exit - quits the program
            """)
          }
          case _ => println("Wrong command")
        }
      } catch {
        case _:NumberFormatException => println("NaN")
        case _:Any => println("Something went wrong")
      }
      command = readLine("> ")
    }
    println("Goodbye!")
  }
}
