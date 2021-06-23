package ir.usc.ac
package insertion_algorithms

import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.scene.text.{Text, TextBoundsType}

import scala.annotation.tailrec
import scala.util.Random

object NQueensInsertion {

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
   * Synchronizes all the components (circle, name and heuristic) based on given x and y for circle center
   * @param component combination of the node circle, node name and node heuristic
   * @param x x axis location for circle center
   * @param y y axis location for circle center
   * @return same components, but synchronized locations
   */
  private def syncLocations(component: (Circle, Text, Text), x: Double, y: Double): (Circle, Text, Text) = {
    val circle = component._1
    val name = component._2
    val heuristic = component._3

    circle.setCenterX(x)
    circle.setCenterX(y)
    name.setX(circle.delegate.getCenterX - 5)
    name.setY(circle.delegate.getCenterY + 5)
    heuristic.setX(circle.delegate.getCenterX - 6)
    heuristic.setY(circle.delegate.getCenterY - 25)

    val returningCircle = Circle(centerX = x, centerY = y, radius = 20, fill = Color.Grey)
    val returningText = new Text(x - 5, y + 5, name.delegate.getText)
    val returningHeuristic = new Text(x - 6, y - 25, heuristic.delegate.getText)
    returningText.setFill(name.delegate.getFill)
    returningText.setBoundsType(TextBoundsType.Visual)
    returningHeuristic.setFill(heuristic.delegate.getFill)
    returningHeuristic.setBoundsType(TextBoundsType.Visual)
    (returningCircle, returningText, returningHeuristic)
  }

  def arrangeComponentsOnScreenUsingNQueensRandomly(
                                                           components: List[(Circle, Text, Text)],
                                                           width: Int,
                                                           height: Int
                                                           ): List[(Circle, Text, Text)] = {
    val allSolutions = nQueens(components.length)
    val randomIndex = Random.nextInt(allSolutions.length)
    arrangeComponentsOnScreenUsingNQueens(
      components, width, height, allSolutions(randomIndex)
    )
  }

  /**
   * Arranges nodes on scree using nQueens random algorithm
   * @param components all the node circles, names and heuristics
   * @param width maximum width to place nodes on
   * @param height maximum height to place nodes on
   *               For example, your screen is 1200 x 800 but you want to place nodes
   *               in a maximum 800 x 600 area
   * @param solution The randomly chosen given nQueens solution
   *                        For example, nQueens gives you 12 solutions,
   *                        One of them is chosen (either randomly or on intend)
   * @return components, arranged on screen
   */
  def arrangeComponentsOnScreenUsingNQueens(
                                             components: List[(Circle, Text, Text)],
                                             width: Int,
                                             height: Int,
                                             solution: List[Int]): List[(Circle, Text, Text)] = {
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
      val yLocation = getY(solution(xIndex))
      syncLocations(component, xLocation, yLocation)
    }

  }

}
