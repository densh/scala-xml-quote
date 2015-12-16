package scala.xml.quote.internal

import scala.Predef.{any2stringadd => _, _}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class QuoteImpl(val c: Context) extends Nodes with Liftables with Unliftables {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication
  assert(args.length == 0)

  def text(): String = parts.head

  def parse(s: String): xml.Node =
    new QuoteParser(io.Source.fromString(s), true).initialize.content(xml.TopScope).head

  def wrap(node: xml.Node) = q"$node"

  def pp[T <: Tree](t: T): T = {
    println(showCode(t))
    t
  }

  def apply[T](args: Tree*): Tree = pp(wrap(parse(text())))
}
