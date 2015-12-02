package scala.xml.quote.internal

import scala.reflect.macros.whitebox.Context

trait Nodes {
  val c: Context
  import c.universe._

  case class Unquote(tree: Tree) extends xml.SpecialNode {
    def label: String = "#UNQUOTE"
    def buildString(sb: StringBuilder): StringBuilder = sb.append(s"{${showCode(tree)}}")
  }
}
