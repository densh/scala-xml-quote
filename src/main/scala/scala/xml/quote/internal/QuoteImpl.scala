package scala.xml.quote.internal

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.xml.quote.internal.QuoteImpl.Hole

class QuoteImpl(val c: whitebox.Context) extends Liftables /*with Unliftables*/ {
  import c.universe._

  lazy val (parts, args) = c.macroApplication match {
    case q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)"  => (parts, args)
    case q"$_($_(..${parts: List[String]})).xmls.apply[..$_](..$args)" => (parts, args)
  }

  def pp[T <: Tree](t: T): T = {
    println(showCode(t))
    t
  }

  def apply[T](args: Tree*): Tree = {
    val nodes = parsedXml
    assert(nodes.size == 1)

    implicit val isTopScope: Boolean = true
    q"${nodes.head}"
  }

  def applySeq[T](args: Tree*): Tree = {
    val nodes = parsedXml
    assert(nodes.size > 1)

    implicit val isTopScope: Boolean = true
    q"$nodes"
  }

  private def parsedXml: Seq[parsed.Node] = {
    val xmlStr = parts.init.zipWithIndex.map {
      case (part, i) => s"$part${Hole(i)}"
    }.mkString("", "", parts.last)

    val parser = new XmlParser
    parser.XmlExpr.parse(xmlStr).get.value
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
