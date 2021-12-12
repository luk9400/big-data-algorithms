// task 1
import scala.collection.mutable.Map
import scala.io.Source

object Graph {
  def mapFunction(line: String): Array[Tuple3[Int, Int, Int]] = {
    val Array(v, u) = line.split(" ").map(_.toInt)
    return Array((v, 0, 1), (u, 1, 0))
  }

  def reduceFunction(
      acc: Map[Int, Tuple2[Int, Int]],
      tup: Tuple3[Int, Int, Int]
  ): Map[Int, Tuple2[Int, Int]] = {
    if (acc.contains(tup._1)) {
      acc(tup._1) = (acc(tup._1)._1 + tup._2, acc(tup._1)._2 + tup._3)
    } else {
      acc(tup._1) = (tup._2, tup._3)
    }

    return acc
  }

  def main(args: Array[String]): Unit = {
    println(args(0))
    val res = Source
      .fromFile(args(0))
      .getLines()
      .flatMap(mapFunction)
      .foldLeft(Map[Int, Tuple2[Int, Int]]())(reduceFunction)

    println(res)
  }
}
