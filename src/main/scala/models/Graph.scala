package ir.usc.ac
package models

case class Graph(nodes: List[Node], connections: List[ConnectionInfo]) {

  private val ifNotExistsMessage = "Connection Info has nodes that do not exist in graph nodes"

  private val nodeNamesFromConnections: List[String] = connections.map(_.from) ++ connections.map(_.to)

  require(nodeNamesFromConnections.distinct.forall(nodeExists), ifNotExistsMessage)

  def nodeExists(name: String): Boolean = nodes.exists(_.name == name)

  def addNode(node: Node): Graph =
    Graph(nodes :+ node, connections)

  override def toString: String = {
    "Graph with nodes:\n\t" + nodes.map(node => s"name: ${node.name}, heuristic: ${node.heuristic}").mkString("- ", "\n\t- ", "") + "\nConnections:\n" +
      connections.mkString("\t", "\n\t", "")
  }

  def getNodeByName(name: String): Option[Node] = {
    nodes.filter(_.name == name).collectFirst {
      case node: Node => node
    }
  }

  def addNodeConnectionByName(from: String, to: String, weight: Int): Graph = {
    val srcOpt: Option[Node] = nodes.find(_.name == from)
    val destinationOpt: Option[Node] = nodes.find(_.name == to)
    srcOpt.flatMap { _ =>
      destinationOpt.map { _ => {
        Graph(nodes = nodes, connections = connections :+ ConnectionInfo(from = from, to = to, weight = weight))
      }}
    }
  }.getOrElse(this)

  def addNodeConnectionByInfo(info: ConnectionInfo): Graph = {
    val srcOpt: Option[Node] = nodes.find(_.name == info.from)
    val destinationOpt: Option[Node] = nodes.find(_.name == info.to)
    srcOpt.flatMap { _ =>
      destinationOpt.map { _ => {
        Graph(nodes = nodes, connections = connections :+ info)
      }}
    }
  }.getOrElse(this)

  def findSpecificNodeConnections(name: String): List[ConnectionInfo] = {
    getNodeByName(name).map { node =>
      connections.filter(connection => connection.from == node.name || connection.to == node.name)
    }
      .getOrElse(List.empty)
  }

  def getDefaultDestinationNode: Option[Node] = {
    nodes.find(_.heuristic == 0)
  }
}

object Graph {
  def empty(): Graph = Graph(List.empty, List.empty)
}
