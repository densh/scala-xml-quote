package scala.xml.quote.internal

import scala.reflect.macros.blackbox

class QuoteImpl(val c: blackbox.Context) extends Liftables /*with Unliftables*/ {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).${_ /* xml(s) */}.apply[..$_](..$args)" = c.macroApplication

  def apply[T](args: Tree*): Tree = {
    val nodes = parsedXml
    assert(nodes.size == 1, "Use xmls with mutiple elements")

    fixScopes(q"${nodes.head}")
  }

  def applySeq[T](args: Tree*): Tree = {
    val nodes = parsedXml
    fixScopes(q"$nodes")
  }

  private def parsedXml: Seq[parsed.Node] = {
    import QuoteImpl.Hole

    val xmlStr = parts.init.zipWithIndex.map {
      case (part, i) => s"$part${Hole.encode(i)}"
    }.mkString("", "", parts.last)

    val parser = new XmlParser
    parser.XmlExpr.parse(xmlStr).get.value
  }

  /** When we lift, we don't know if we are within an enclosing xml element
    * which defines a scope. In some cases we will have to fix the scope.
    *
    * E.g:
    * {{{
    *   xml"""<a xmlns:pre="scope0">${ xml"<b/>" }</a>"""
    * }}}
    * Here the scope of `b` is `TopScope` but should be `scope0`
    */
  private def fixScopes(tree: Tree): Tree = {
    val typed = c.typecheck(tree)

    var scopeSym = NoSymbol
    c.internal.typingTransform(typed)((tree, api) => tree match {
      case q"$_.TopScope" if scopeSym != NoSymbol =>
        api.typecheck(q"$scopeSym")
      case q"val $$scope$$ = $_" => // this assignment is only here when creating new scope
        scopeSym = tree.symbol
        api.default(tree)
      case _ =>
        api.default(tree)
    })
  }

  def pp[T <: Tree](t: T): T = {
    println(showCode(t))
    t
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
