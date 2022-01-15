package ir.usc.ac

import models.{ConnectionInfo, Graph, Node}
import algorithm.{AStarAlgorithmRunner, AStarBuilder}

import java.time.LocalTime
import java.time.temporal.ChronoUnit

object Run extends App {

  val homeDirectory: String = System.getenv("HOME")

  val nodes: List[Node] = Node.fromFile(homeDirectory + "/Desktop/Projects/AStarNodesInfo/nodes.nodeInfo")

  val connectionsInfo: List[ConnectionInfo] = ConnectionInfo.fromFile(homeDirectory + "/Desktop/Projects/AStarNodesInfo/graph.connectionInfo")

  val thisGraph: Graph = models.Graph(nodes, connectionsInfo)

  val algorithmRunner: AStarAlgorithmRunner =
    (AStarBuilder in thisGraph from "A" to "J").build()

  lazy val path: IndexedSeq[String] = algorithmRunner.run()

  val start: LocalTime = LocalTime.now()
  path
  val finish: LocalTime = LocalTime.now()
  val totalTime: Long = ChronoUnit.MILLIS.between(start, finish)

  val indexedPath: IndexedSeq[(String, String)] = path.drop(1).zipWithIndex.map {
    case (currentNode, lastNodeIndex) =>
      path(lastNodeIndex) -> currentNode
  }

  val graphDescription = indexedPath.map { traverse =>
    val (srcNode, dstNode) = traverse
    s"from `$srcNode` to `$dstNode`"
  }.mkString(", and then ") + s" in $totalTime milli seconds"

  println(graphDescription)

}
