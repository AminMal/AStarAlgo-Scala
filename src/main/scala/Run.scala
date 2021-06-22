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

  /**
   *
   * @param n number of nodes, so we can give sorting and insertion solutions based on number of nodes
   * @return List of all the solutions for n nodes on the screen
   */
  def nQueens(n: Int): List[List[Int]] = {
    def hasConflict(position: Int, queens: List[Int]): Boolean = {
      def hasConflictForSingleQueen(position: Int, queen: Int, index: Int): Boolean =
        queen == position || (index + 1) == (queen - position) || (index + 1) == (position - queen)

      queens.zipWithIndex.exists { pair =>
        val (queen, index) = pair
        hasConflictForSingleQueen(position, queen, index)
      }
    }

    @tailrec
    def nQueensTailrec(currentPosition: Int,
                       currentQueens: List[Int] = List(),
                       solutions: List[List[Int]] = List()): List[List[Int]] = {
      if (currentPosition >= n && currentQueens.isEmpty) solutions
      else if (currentPosition >= n) {
        nQueensTailrec(currentQueens.head + 1, currentQueens.tail, solutions)
      } else if (hasConflict(currentPosition, currentQueens)) {
        nQueensTailrec(currentPosition + 1, currentQueens, solutions)
      } else if (currentQueens.length == n - 1) {
        val newSolution = currentPosition :: currentQueens
        nQueensTailrec(currentPosition + 1, currentQueens, newSolution :: solutions)
      } else nQueensTailrec(0, currentPosition :: currentQueens, solutions)
    }

    nQueensTailrec(0)
  }

  /**
   *
   * @param list the input list you want to swap your elements from
   * @param firstIndex one of the indexes you want to swap, doesn't matter if is smaller or bigger than the other
   * @param secondIndex the other index to swap
   * @tparam T type of list elements
   * @return same list, but swapped elements
   * @throws IndexOutOfBoundsException in case indexes are out of list bound
   */
  def swap[T](list: List[T], firstIndex: Int, secondIndex: Int): List[T] = {
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
   * @param list input list to randomize
   * @param n number of random swaps
   * @tparam T list's values type
   * @return same list, but swapped n random elements randomly
   */
  @tailrec
  def swapNRandomElementsIn[T](list: List[T], n: Int): List[T] = {
    val listLength = list.length
    if (n <= 0) list
    else {
      val from = Random.nextInt(listLength)
      var to = Random.nextInt(listLength)
      while(to == from) {
        to = Random.nextInt(listLength)
      }
      val swapped = swap(list, from, to)
      swapNRandomElementsIn(swapped, n - 1)
    }
  }

  /**
   * Synchronizes all the components (circle, name and heuristic) based on given x and y for circle center
   * @param component combination of the node circle, node name and node heuristic
   * @param x x axis location for circle center
   * @param y y axis location for circle center
   * @return same components, but synchronized locations
   */
  def syncLocations(component: (Circle, Text, Text), x: Double, y: Double): (Circle, Text, Text) = {
    val circle = component._1
    val name = component._2
    val heuristic = component._3

    circle.setCenterX(x)
    circle.setCenterX(y)
    name.setX(circle.delegate.getCenterX - 5)
    name.setY(circle.delegate.getCenterY + 5)
    heuristic.setX(circle.delegate.getCenterX - 6)
    heuristic.setY(circle.delegate.getCenterY - 25)

    val rc = Circle(centerX = x, centerY = y, radius = 20, fill = Color.Grey)
    val t = new Text(x - 5, y + 5, name.delegate.getText)
    val h = new Text(x - 6, y - 25, heuristic.delegate.getText)
    t.setFill(name.delegate.getFill)
    t.setBoundsType(TextBoundsType.Visual)
    h.setFill(heuristic.delegate.getFill)
    h.setBoundsType(TextBoundsType.Visual)
    (rc, t, h)
  }

  /**
   * Arranges nodes on scree using nQueens random algorithm
   * @param components all the node circles, names and heuristics
   * @param width maximum width to place nodes on
   * @param height maximum height to place nodes on
   *               For example, your screen is 1200 x 800 but you want to place nodes
   *               in a maximum 800 x 600 area
   * @param basedOnSolution The randomly chosen given nQueens solution
   *                        For example, nQueens gives you 12 solutions,
   *                        One of them is chosen (either randomly or on intend)
   * @return components, arranged on screen
   */
  def arrangeComponentsOnScreen(
                                 components: List[(Circle, Text, Text)],
                                 width: Int,
                                 height: Int,
                                 basedOnSolution: List[Int]): List[(Circle, Text, Text)] = {
    val numberOfComponents = components.length
    val eachNodeWidthOffset = width / numberOfComponents
    val eachNodeHeightOffset = height / numberOfComponents

    def getX(index: Int, xOffset: Int = eachNodeWidthOffset): Double = {
      (index * xOffset) + (xOffset / 2)
    }

    def getY(index: Int, yOffset: Int = eachNodeHeightOffset): Double = {
      50 + (index * yOffset) + (yOffset / 2)
    }

    components.zipWithIndex.map { pair =>
      val (component, xIndex) = pair
      val xLocation = getX(xIndex)
      val yLocation = getY(basedOnSolution(xIndex))
      syncLocations(component, xLocation, yLocation)
    }

  }

  // End of nQueens-ish algorithms and placements

  /**
   * Given all the components, returns circle center location for a given node name
   * @param components nodes (circles, names, heuristics)
   * @param nodeName the name you want to find locations for
   * @return x and y location of that node, IF exists (Optional)
   */
  def findNodeLocationInComponents(
                                  components: List[(Circle, Text, Text)],
                                  nodeName: String
                                  ): Option[(Double, Double)] = {
    components.map(p => (p._1, p._2.delegate.getText))
      .find(circleAndText => circleAndText._2 == nodeName).map { existingNode =>
      (existingNode._1.delegate.getCenterX, existingNode._1.delegate.getCenterY)
    }
  }

  /**
   * returns x and y location of index-th node of the components
   * @param index index of the node
   * @param radius radius of the circle to be shown on the screen
   * @param circleCenterX circle that nodes are going to be placed on x location
   * @param circleCenterY circle that nodes are going to be placed on y location
   * @param totalNodesCount total number of nodes in components to be placed on the circle
   * @return x and y location to insert node on
   */
    def calculateNodeLocationOnCircle(index: Int,
                                      radius: Double,
                                      circleCenterX: Double,
                                      circleCenterY: Double,
                                      totalNodesCount: Int): (Double, Double) = {
      val unitAngle = 360.0 / totalNodesCount
      val thisNodeAngle: Double = unitAngle * index
      val returningX = circleCenterX + (Math.cos(Math.toRadians(thisNodeAngle)) * radius)
      val returningY = circleCenterY + (Math.sin(Math.toRadians(thisNodeAngle)) * radius)
      (returningX, returningY)
    }

  /**
   * components placed in the right screen location
   * @param components all the components to be placed (node circles, names and heuristics)
   * @param screenCenterX circle to place nodes on, x location
   * @param screenCenterY circle to place nodes on, y location
   * @return all the components, placed in the right location
   */
  def arrangeComponentsOnCircle(
                               components: List[(Circle, Text, Text)],
                               screenCenterX: Double,
                               screenCenterY: Double
                               ): List[(Circle, Text, Text)] = {
    val totalNodesCount = components.length
    val componentsAndRelatedLocations = components.zipWithIndex.map { pair =>
      val (component, index) = pair
      (component, calculateNodeLocationOnCircle(index, radius = 275, screenCenterX, screenCenterY, totalNodesCount = totalNodesCount))
    }

    componentsAndRelatedLocations.map { pair =>
      val (component, relatedLocation) = pair
      val (xLocation, yLocation) = relatedLocation
      val circleToReturn = Circle(centerX = xLocation, centerY = yLocation, radius = 20.0)
      circleToReturn.setFill(component._1.delegate.getFill)
      circleToReturn.setViewOrder(-1)
      val nameToReturn = new Text(xLocation - 5, yLocation + 5, component._2.getText)
      nameToReturn.setFill(component._2.delegate.getFill)
      nameToReturn.setViewOrder(-2)
      val heuristicToReturn = new Text(xLocation - 6, yLocation - 25, component._3.delegate.getText)
      heuristicToReturn.setFill(component._3.delegate.getFill)
      heuristicToReturn.setViewOrder(-2)
      (circleToReturn, nameToReturn, heuristicToReturn)
    }
  }

  val homeDirectory = System.getenv("HOME")

  val nodes: List[Node] = Node.fromFile(homeDirectory + "/Desktop/nodes.nodeInfo")

  val connectionsInfo: List[ConnectionInfo] = ConnectionInfo.fromFile(homeDirectory + "/Desktop/graph.connectionInfo")

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
    val randomizedNodes: List[(Circle, Text, Text)] = swapNRandomElementsIn(initial, Math.sqrt(listLength -1).round.toInt)

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
