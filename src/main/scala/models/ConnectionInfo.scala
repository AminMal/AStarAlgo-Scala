package ir.usc.ac
package models

import java.util.regex.Pattern
import scala.io.Source

case class ConnectionInfo(from: String, to: String, weight: Int) {
  override def toString: String =
    s"$from ==-$weight-==> $to"
}

object ConnectionInfo {

  private val pattern: Pattern = Pattern.compile("\\s*[a-zA-Z]+\\s*[|]\\s*[a-zA-Z]+\\s*[|].*")

  def fromFile(srcFile: String): List[ConnectionInfo] = {
    val fileContent = Source.fromFile(name = srcFile, enc = "UTF-8")
    fileContent.getLines().filterNot(l => Pattern.compile("\\s*").matcher(l).matches()).map { line =>
      val patternMatches = pattern.matcher(line).matches()
      if (patternMatches) {
        val splitByBar = line.replaceAll(" ", "").split("[|]")
        val sourceNode = splitByBar(0)
        val destinationNode = splitByBar(1)
        val weight = try {
          splitByBar(2).toInt
        } catch {
          case _: Exception =>
            throw new RuntimeException(s"Can not match '${splitByBar(2)}' as Integer")
        }

        val result = ConnectionInfo(sourceNode, destinationNode, weight)
        Some(result)
      } else {
        println(s"line: *$line*\nfrom file: $srcFile doesn't match .connectionInfo files syntax, therefore will be skipped")
        println(pattern)
        None
      }
    }.filter(_.isDefined).map(_.get).toList
  }
}
