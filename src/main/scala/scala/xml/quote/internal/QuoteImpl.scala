package scala.xml.quote.internal

import scala.reflect.macros.whitebox
import QuoteImpl._
import scala.xml.quote.internal.parsed.Placeholder

class QuoteImpl(val c: whitebox.Context) extends Liftables {
  import c.universe._

  lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication

  def apply[T](args: Tree*): Tree = {
    val nodes = parsedXml

    val tree =
      if (nodes.size == 1) q"${nodes.head}"
      else q"$nodes"

    fixScopes(tree)
  }

  private def parsedXml: Seq[parsed.Node] = {

    val xmlStr = parts.init.zipWithIndex.map {
      case (part, i) => s"$part${Hole.encode(i)}"
    }.mkString("", "", parts.last)


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

private object QuoteImpl {
  import fastparse.all._
  val parser = new XmlParser(Hole.Parser.map(Placeholder))
}
