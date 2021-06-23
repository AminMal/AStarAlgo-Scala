package ir.usc.ac

import models.{ConnectionInfo, Graph, Node}

import algorithm.{AStarAlgorithmRunner, AStarBuilder}
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.text.{Text, TextAlignment, TextBoundsType}

import java.time.LocalTime
import scala.annotation.tailrec
import scala.util.Random

object Run extends JFXApp {

  import insertion_algorithms.CircleInsertion._

  implicit class ListSwapper[T](list: List[T]) {
    /**
     *
     * @param firstIndex one of the indexes you want to swap, doesn't matter if is smaller or bigger than the other
     * @param secondIndex the other index to swap
     * @return same list, but swapped elements
     * @throws IndexOutOfBoundsException in case indexes are out of list bound
     */
    def swap(firstIndex: Int, secondIndex: Int): List[T] = {
      val lowerIndex = Math.min(firstIndex, secondIndex)
      val higherIndex = Math.max(firstIndex, secondIndex)
      val middleSlice: List[T] = list.slice(lowerIndex + 1, higherIndex)
      val firstSlice: List[T] = list.slice(0, lowerIndex)
      val secondSlice: List[T] = list.takeRight(list.length - (higherIndex + 1))
      val firstElem: T = list(lowerIndex)
      val secondElem: T = list(higherIndex)
      (firstSlice :+ secondElem) ++ middleSlice ++ (firstElem +: secondSlice)
    }

    /**
     * Swaps n random elements, for example List(1, 2, 3, 4, 5, 6, 7, 8)
     * getting swapped 2 times with randomly chosen indexes could convert to:
     * List(2, 1, 3, 6, 5, 4, 7, 8) where 1 and 2, and 4 an 6 are swapped
     * @param n number of random swaps
     * @return same list, but swapped n random elements randomly
     */

    @tailrec
    final def swapElementsNRandomTimes(n: Int): List[T] = {
      val listLength = list.length
      if (n <= 0) list
      else {
        val from = Random.nextInt(listLength)
        var to = Random.nextInt(listLength)
        while(to == from) {
          to = Random.nextInt(listLength)
        }
        val swapped = list.swap(from, to)
        swapped.swapElementsNRandomTimes(n - 1)
      }
    }

  }

  val homeDirectory: String = System.getenv("HOME")

  val nodes: List[Node] = Node.fromFile(homeDirectory + "/Desktop/Projects/AStarNodesInfo/nodes.nodeInfo")

  val connectionsInfo: List[ConnectionInfo] = ConnectionInfo.fromFile(homeDirectory + "/Desktop/Projects/AStarNodesInfo/graph.connectionInfo")

  val graph: Graph = models.Graph(nodes, connectionsInfo)

  val algorithmRunner: AStarAlgorithmRunner = AStarBuilder
    .in(graph)
    .from("A")
    .to("J")
    .build()

  lazy val path: IndexedSeq[String] = algorithmRunner.run()

  val start: LocalTime = LocalTime.now()
  path
  val finish: LocalTime = LocalTime.now()
  val totalTime: Int = (finish.getNano - start.getNano) / 1000000

  stage = new JFXApp.PrimaryStage {
    title = "A* algorithm in Scala!"
    val initial: List[(Circle, Text, Text)] = nodes.map { node =>
      val name = new Text(node.name)
      name.setBoundsType(TextBoundsType.Visual)
      val circleX = Random.nextInt(600) + 100
      val circleY = Random.nextInt(400) + 100
      val c = Circle(centerX = circleX, centerY = circleY, radius = 20, fill = Color.Grey)
      name.setX(c.delegate.getCenterX - 5)
      name.setY(c.delegate.getCenterY + 5)
      name.setFill(Color.White)
      val heuristic = new Text(node.heuristic.toString)
      heuristic.setBoundsType(TextBoundsType.Visual)
      heuristic.setX(c.delegate.getCenterX - 6)
      heuristic.setY(c.delegate.getCenterY - 25)
      heuristic.setFill(Color.Red)
      (c, name, heuristic)
    }

    val listLength: Int = initial.length
    val randomizedNodes: List[(Circle, Text, Text)] = initial.swapElementsNRandomTimes(Math.sqrt(listLength -1).round.toInt)

    // if nQueens approach was wanted, this would've been the bomb!
//    val allSolutions: List[List[Int]] = nQueens(listLength)
//    val randomChosenSolution: Int = Random.nextInt(listLength)
//    val solution = allSolutions(randomChosenSolution)

    val finalNodes: List[(Circle, Text, Text)] = arrangeComponentsOnCircle(randomizedNodes, 650, 370)

    val findNodeLocationsInFinalNodes: String => Option[(Double, Double)] =
      findNodeLocationInComponents(finalNodes, _)

    val verbosePath: IndexedSeq[(String, String)] = path.drop(1).zipWithIndex.map { pair =>
      val (thisNode, lastNodeIndex) = pair
      val from = path(lastNodeIndex)
      (from, thisNode)
    }

    val accurateAnswerPathLocations: List[((Double, Double), (Double, Double))] = verbosePath
      .map(ss => (findNodeLocationsInFinalNodes(ss._1), findNodeLocationsInFinalNodes(ss._2)))
      .filter(a => a._1.isDefined && a._2.isDefined)
      .map(a => (a._1.get, a._2.get)).toList

    val answerLines: List[Line] = accurateAnswerPathLocations.map { lineLocation =>
      val ((startXLocation, startYLocation), (endXLocation, endYLocation)) = lineLocation
      val line = Line(startXLocation, startYLocation, endXLocation, endYLocation)
      line.setFill(Color.Red)
      line.setStroke(Color.Red)
      line.setStrokeWidth(4)
      line.setViewOrder(0)
      line
    }

    val graphDescriptionStr: String = verbosePath.map { travers =>
      val (srcNode, dstNode) = travers
      s"from `$srcNode` to `$dstNode`"
    }.mkString(", and then ") + s" in $totalTime milli seconds"

    val graphDescriptionText = new Text(330, 50, graphDescriptionStr)
    graphDescriptionText.setFill(Color.Black)
    graphDescriptionText.setBoundsType(TextBoundsType.Visual)
    graphDescriptionText.setTextAlignment(TextAlignment.Center)

    val connectionsAsLineCoordinates = connectionsInfo.map { connection =>
      val source = connection.from
      val destination = connection.to
      val sourceLocation = findNodeLocationsInFinalNodes(source)
      val destinationLocation = findNodeLocationsInFinalNodes(destination)
      val weight = sourceLocation.flatMap { sLocation =>
        destinationLocation.map { dLocation =>
          ((sLocation._1 + dLocation._1) / 2, (sLocation._2 + dLocation._2) / 2)
        }
      }
      val returningWeight = weight.map { location =>
        val weightText = new Text(location._1, location._2, connection.weight.toString)
        weightText.setFill(Color.Black)
        weightText.setBoundsType(TextBoundsType.Visual)
        weightText.setViewOrder(-1)
        weightText
      }
      (sourceLocation, destinationLocation, returningWeight)
    }

    val lines: List[(Line, Text)] = connectionsAsLineCoordinates.filter(p => p._1.isDefined && p._2.isDefined && p._3.isDefined)
      .map(p => (p._1.get, p._2.get, p._3.get))
      .map { location =>
        val ((startXLocation, startYLocation), (endXLocation, endYLocation)) = (location._1, location._2)
        val line = Line(startXLocation, startYLocation, endXLocation, endYLocation)
        line.setFill(Color.Grey)
        line.setStrokeWidth(3)
        line.setStroke(Color.Grey)
        (line, location._3)
      }

    scene = new Scene(width = 1200, height = 800) {
      content = finalNodes.map(_._1) ++ finalNodes.map(_._2) ++ finalNodes.map(_._3) ++ lines.map(_._1) ++ lines.map(_._2) ++
        answerLines :+ graphDescriptionText
    }
    scene.value.setFill(Color(0.3, 0.2, 0.2, 0.15))
  }

}
