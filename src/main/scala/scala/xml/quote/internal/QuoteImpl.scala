package scala.xml.quote.internal

import scala.Predef.{any2stringadd => _, _}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class QuoteImpl(val c: Context) extends Nodes with Liftables with Unliftables {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication

  private lazy val exprIt = args.iterator

  def parse(s: String): xml.Node = {
    val parser = new QuoteParser(io.Source.fromString(s), true, _ => Unquote(exprIt.next()))
    parser.initialize.content(xml.TopScope).head
  }

  def wrap(node: xml.Node) = q"$node"

  def pp[T <: Tree](t: T): T = {
    println(showCode(t))
    t
  }

  def apply[T](args: Tree*): Tree = {
    val text = parts.map(_.replace("$", "$$")).mkString("$")
    pp(wrap(parse(text)))
  }
}
