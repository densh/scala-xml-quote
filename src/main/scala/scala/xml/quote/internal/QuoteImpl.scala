package scala.xml.quote.internal

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.xml.SpecialNode

class QuoteImpl(val c: Context) extends Nodes with Liftables with Unliftables {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication

  def parse(ss: Seq[String]): xml.Node =
    new QuoteParser(ss map io.Source.fromString, true).initialize.content(xml.TopScope).head

  def wrap(node: xml.Node) = q"$node"

  def apply[T](args: Tree*): Tree = wrap(parse(parts))
}
