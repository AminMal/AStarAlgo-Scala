package ir.usc.ac
package models

import java.util.regex.Pattern
import scala.io.Source

case class Node(name: String, heuristic: Long)

object Node {
  private val pattern: Pattern = Pattern.compile(s"\\s*name\\s*=\\s*[^|]+\\s*[|]\\s*heuristic\\s*=\\s*[0-9]{1,5}\\s*")

  /**
   * Returns list of nodes in a given file
   * @param srcFile source file address
   * @return list of nodes based on the input file
   */
  def apply(srcFile: String): List[Node] = {
    val file = Source.fromFile(name = srcFile, enc = "UTF-8")
    // lines that start with `#` are considered as comments
    val nodes = file.getLines().filterNot(_.startsWith("#")).map { line =>
      if (pattern.matcher(line).matches()) {
        val separated = line.split("[|]").toList
        val namePair = separated.head
        val heuristicPair = separated(1)

        val name = namePair.split("[=]\\s*")(1).replaceAll(" ", "")
        val heuristic = heuristicPair.split("=\\s*")(1).replaceAll(" ", "").toInt

        Some(Node(name, heuristic))
      } else {
        println("line: " + line + "doesn't match")
        None
      }
    }.toList

    file.close()
    val fileHasCorruptedInput = nodes.exists(_.isEmpty)
    if (fileHasCorruptedInput) {
      throw new RuntimeException("Source file has corrupted input")
    }
    val existingNodes = nodes.filter(_.isDefined).map(_.get)
    existingNodes

  }

  /**
   * same as apply, but naming makes more sense
   * @param srcFile source file address
   * @return list of nodes in the file
   */
  def fromFile(srcFile: String): List[Node] = this(srcFile)

}
