package scala.xml.quote.internal

import scala.Predef.{any2stringadd => _, _}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.xml.quote.internal.QuoteImpl.Hole

class QuoteImpl(val c: Context) extends Nodes with Liftables with Unliftables {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication

  def parse(s: String): xml.NodeBuffer = {
    val parser = new QuoteParser(io.Source.fromString(s), true)
    val nodes = parser.initialize.content(xml.TopScope)
    val buffer = new xml.NodeBuffer
    nodes.foreach(buffer &+ _)
    buffer
  }

  def wrap(buf: xml.NodeBuffer) = {
    if (buf.size == 1) q"${buf.head}"
    else q"$buf"
  }


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
    private val char = 0xE000.toChar // withing private use area

    def isScalaExpr(c: Char): Boolean = c == char

    def apply(i: Int): String = char.toString * (i + 1)

    def unapply(s: String): Option[Int] =
      if (s.forall(isScalaExpr)) Some(s.length - 1)
      else None
  }
}
