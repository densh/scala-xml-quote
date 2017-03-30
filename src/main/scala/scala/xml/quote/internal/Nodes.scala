package scala.xml.quote.internal

import scala.reflect.macros.blackbox

trait Nodes {
  val c: blackbox.Context
  import c.universe._

  case class Unquote(tree: Tree) extends xml.SpecialNode {
    def label: String = "#UNQUOTE"
    def buildString(sb: StringBuilder): StringBuilder = sb.append(s"{${showCode(tree)}}")
  }
}
