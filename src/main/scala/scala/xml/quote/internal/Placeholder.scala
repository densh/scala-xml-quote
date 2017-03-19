package scala.xml.quote.internal

import scala.xml.SpecialNode

object Placeholder extends SpecialNode {
  override def buildString(sb: StringBuilder): StringBuilder = sb
  override def label: String = "HOLE"
}
