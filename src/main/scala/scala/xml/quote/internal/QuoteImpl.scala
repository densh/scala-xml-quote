package scala.xml.quote.internal

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.xml.quote.internal.QuoteImpl.Hole

class QuoteImpl(val c: blackbox.Context) extends Liftables /*with Unliftables*/ {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).${_ /* xml(s) */}.apply[..$_](..$args)" = c.macroApplication

  def pp[T <: Tree](t: T): T = {
    println(showCode(t))
    t
  }

  def apply[T](args: Tree*): Tree = {
    val nodes = parsedXml
    assert(nodes.size == 1)

    implicit val isTopScope: Boolean = true
    pp(q"${nodes.head}")
  }

  def applySeq[T](args: Tree*): Tree = {
    val nodes = parsedXml
    assert(nodes.size > 1)

    implicit val isTopScope: Boolean = true
    q"$nodes"
  }

  private def parsedXml: Seq[parsed.Node] = {
    val xmlStr = parts.init.zipWithIndex.map {
      case (part, i) => s"$part${Hole.encode(i)}"
    }.mkString("", "", parts.last)

    val parser = new XmlParser
    parser.XmlExpr.parse(xmlStr).get.value
  }
}

object QuoteImpl {

  private[internal] object Hole {
    private val char = 0xE000.toChar // withing private use area

    def isScalaExpr(c: Char): Boolean = c == char

    def encode(i: Int): String = char.toString * (i + 1)

    def decode(s: String): Option[Int] = {
      if (s.forall(isScalaExpr)) Some(s.length - 1)
      else None
    }

  }
}
