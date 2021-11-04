object EdgesInversion {
  type Graph = Array[(Int, Array[Int])]

  def printGraph(graph: Graph): Unit = {
    println(graph.map(x => s"${x._1} [${x._2.mkString(", ")}]") mkString "\n")
  }

  def edgesInversion(graph: Graph): Graph = {
    graph
      .flatMap(item => item._2.map(elem => (elem, Array(item._1))))
      .groupMapReduce(_._1)(_._2)((a, b) => a ++ b) // use in scala 2.13
      .toArray
  }

  def main(args: Array[String]): Unit = {
    val graph =
      Array(
        (1, Array(2, 3)),
        (3, Array(1, 5)),
        (2, Array(5)),
        (5, Array[Int]())
      )

    printGraph(graph)
    println("Reversed:")
    printGraph(edgesInversion(graph))
  }
}
