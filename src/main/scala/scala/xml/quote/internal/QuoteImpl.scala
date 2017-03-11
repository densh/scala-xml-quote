package scala.xml.quote.internal

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.xml.Atom
import scala.xml.quote.internal.QuoteImpl.Hole

class QuoteImpl(val c: Context) extends Nodes with Liftables with Unliftables {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication

  def parse(ss: Seq[String]): xml.Node =
    new QuoteParser(ss map io.Source.fromString, args.zipWithIndex.map(x => new Hole(x._2)), true).initialize.content(xml.TopScope).head

  def wrap(node: xml.Node) = q"$node"

  def pp[T <: Tree](t: T): T = {
    println(showCode(t))
    t
  }

  def apply[T](args: Tree*): Tree = pp(wrap(parse(parts)))
}

object QuoteImpl {
  class Hole(data: Int) extends Atom[Int](data)
}
