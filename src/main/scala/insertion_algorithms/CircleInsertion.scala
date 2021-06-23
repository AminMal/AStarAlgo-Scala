package ir.usc.ac
package insertion_algorithms

import scalafx.scene.shape.Circle
import scalafx.scene.text.Text

object CircleInsertion {

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

}
