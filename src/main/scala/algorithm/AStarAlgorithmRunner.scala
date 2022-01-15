package ir.usc.ac
package algorithm

import models.Graph

import scala.annotation.tailrec

sealed case class AStarAlgorithmRunner(from: Option[String], to: Option[String], graph: Graph) {

  object NodeSelectionOrdering extends Ordering[(String, Long)] {
    override def compare(x: (String, Long), y: (String, Long)): Int = x._2 compare y._2
  }

  private val startRequirement = from.flatMap { f =>
    to.map { t =>
      graph.nodeExists(f) && graph.nodeExists(t)
    }
  }.getOrElse(false)

  @tailrec
  private def verboseHelper(path: IndexedSeq[String], lastNode: String, previousNode: Option[String]): IndexedSeq[String] = {
    val lastNodeConnections = if (path.isEmpty) {
      graph.findSpecificNodeConnections(lastNode)
    } else {
      graph.findSpecificNodeConnections(lastNode).filterNot(c => c.from == previousNode.get || c.to == previousNode.get)
    }
    val a = lastNodeConnections.flatMap { connection =>
      val otherNode = if (connection.from == lastNode) {
        connection.to
      } else {
        connection.from
      }
      val destinationNode = graph.getNodeByName(otherNode)
      destinationNode.map { destination =>
        val weight = destination.heuristic + connection.weight
        (destination.name, weight)
      }
    }
    println(s"First choice is:\nstarting from $lastNode which one should we choose? ")
    println(a.mkString("" ," or ", "?"))
    val minimumPathSelection = a.min(NodeSelectionOrdering)
    println(s"Of course $minimumPathSelection")
    if (minimumPathSelection._1 == to.get) {
      path.appended(minimumPathSelection._1)
    } else {
      verboseHelper(path.appended(minimumPathSelection._1), minimumPathSelection._1, Some(lastNode))
    }
  }

  @tailrec
  private def helper(path: IndexedSeq[String], lastNode: String, previousNode: Option[String]): IndexedSeq[String] = {
    val lastNodeConnections = if (path.isEmpty) {
      graph.findSpecificNodeConnections(lastNode)
    } else {
      graph.findSpecificNodeConnections(lastNode).filterNot(c => c.from == previousNode.get || c.to == previousNode.get)
    }
    val a = lastNodeConnections.flatMap { connection =>
      val otherNode = if (connection.from == lastNode) {
        connection.to
      } else {
        connection.from
      }
      val destinationNode = graph.getNodeByName(otherNode)
      destinationNode.map { destination =>
        val weight = destination.heuristic + connection.weight
        (destination.name, weight)
      }
    }
    val minimumWeightedDestination = a.min(NodeSelectionOrdering)
    if (minimumWeightedDestination._1 == to.get) {
      path.appended(minimumWeightedDestination._1)
    } else {
      helper(path.appended(minimumWeightedDestination._1), minimumWeightedDestination._1, Some(lastNode))
    }
  }

  def run(): IndexedSeq[String] = {
    if (!startRequirement) {
      throw new RuntimeException("Early start or invalid `from` and `to` nodes")
    } else {
      IndexedSeq(from.get).appendedAll(helper(IndexedSeq(), from.get, None))
    }
  }

  def runPrintingChoices(): IndexedSeq[String] = {
    if (!startRequirement) {
      throw new RuntimeException("Early start or invalid `from` and `to` nodes")
    } else {
      println(s"Stats: from ${from.get} to ${to.get}, graph: \n$graph")
      IndexedSeq(from.get).appendedAll(verboseHelper(IndexedSeq(), from.get, None))
    }
  }
}

object AStarBuilder {
  private var starter: AStarAlgorithmRunner = AStarAlgorithmRunner(None, None, Graph.empty())

  private var graphIsGiven: Boolean = false

  private var startingNodeIsGiven: Boolean = false

  private var endingNodeIsGiven: Boolean = false

  def in(graph: Graph): AStarBuilder.type = {
    starter = AStarAlgorithmRunner(starter.from, starter.to, graph)
    graphIsGiven = true
    this
  }

  def from(node: String): AStarBuilder.type = {
    starter = AStarAlgorithmRunner(Some(node), starter.to, starter.graph)
    startingNodeIsGiven = true
    this
  }

  def to(node: String): AStarBuilder.type = {
    starter = AStarAlgorithmRunner(starter.from, Some(node), starter.graph)
    endingNodeIsGiven = true
    this
  }

  def build(): AStarAlgorithmRunner =
    if (graphIsGiven && startingNodeIsGiven && endingNodeIsGiven) {
      starter
    } else {
      val exceptionMessage: String = {
        if (!graphIsGiven) {
          "Graph is not given"
        } else if (!startingNodeIsGiven) {
          "Starting node is not given"
        } else {
          "ending node is not given"
        }
      }
      throw new RuntimeException(exceptionMessage)
    }

  def toDefault: AStarAlgorithmRunner =
    starter.graph.getDefaultDestinationNode
      .map { existingNode =>
        AStarAlgorithmRunner(starter.from, Some(existingNode.name), starter.graph)
      }.getOrElse(throw new RuntimeException("No node starts with 0 heuristic in file"))
}
