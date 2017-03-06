package scala.xml.quote.internal

import scala.Predef.{any2stringadd => _, _}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.xml.quote.internal.QuoteImpl.Hole

class QuoteImpl(val c: Context) extends Nodes with Liftables with Unliftables {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication

  def parse(s: String): xml.Node = {
    val parser = new QuoteParser(io.Source.fromString(s), true)
    parser.initialize.content(xml.TopScope).head
  }

  def wrap(node: xml.Node) = q"$node"

  def pp[T <: Tree](t: T): T = {
    println(showCode(t))
    t
  }

  def apply[T](args: Tree*): Tree = {
    val xml = parts.init.zipWithIndex.map {
      case (part, i) => s"$part${Hole(i)}"
    }.mkString("", "", parts.last)
    pp(wrap(parse(xml)))
  }
}

object QuoteImpl {

  private[internal] object Hole {
    private val pat = java.util.regex.Pattern.compile("^\\$(\\d+)$")

    def apply(i: Int) = s"$$$i"

    def unapply(s: String): Option[Int] = {
      val m = pat.matcher(s)
      if (m.find()) Some(m.group(1).toInt) else None
    }
  }
}
