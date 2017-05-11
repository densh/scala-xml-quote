package scala.xml.quote
package internal

import fastparse.all._

import scala.xml.parsing.TokenTests
import internal.{parsed => p}

// FIXME Name should not end by :
// FIXME tag must be balanced
private[internal] class XmlParser(Hole: P[p.Placeholder]) extends TokenTests {

  private val WL = CharsWhile(_.isWhitespace).opaque("whitespace")

  val XmlExpr: P[Seq[p.Node]] = P( WL.? ~ Xml.XmlContent.rep(min = 1, sep = WL.?) ~ WL.? ~ End )
  val XmlPattern: P[p.Node]   = P( WL.? ~ Xml.ElemPattern ~ WL.? ~ End)

  private[this] object Xml {

    val Element: P[p.Node] = P( TagHeader ~/ (EmptyElemTagEnd | ">" ~/ Content ~/ ETag ) ).map {
      case (qname, atts, children: Seq[p.Node @unchecked]) => p.Node(qname, atts, minimizeEmpty = false, children)
      case (qname, atts, _)                                => p.Node(qname, atts, minimizeEmpty = true, Nil)
    }
    val TagHeader        = P( "<" ~ Name.! ~/ (WL ~ Attribute).rep ~ WL.? )
    val EmptyElemTagEnd  = P( "/>" )
    val ETag             = P( "</" ~ Name ~ WL.? ~ ">" )

    val Attribute = P( Name.! ~ Eq ~ AttValue ).map {
      case (qname, sc: p.Placeholder) => p.Attribute(qname, sc)
      case (qname, value: String)   => p.Attribute(qname, value)
    }
    val Eq       = P( WL.? ~ "=" ~ WL.? )
    val AttValue = P(
      "\"" ~/ (CharQ | Reference).rep.! ~ "\"" |
      "'" ~/ (CharA | Reference).rep.! ~ "'" |
      ScalaExpr
    )

    val Content               = P( (CharData | Reference | ScalaExpr | XmlContent).rep )
    val XmlContent: P[p.Node] = P( Unparsed | CDSect | PI | Comment | Element )

    val ScalaExpr = Hole

    val Unparsed = P( UnpStart ~/ UnpData.! ~ UnpEnd ).map(p.Unparsed)
    val UnpStart = P( "<xml:unparsed" ~ (WL ~ Attribute).rep ~ WL.? ~ ">" ).map(_ => Unit): P0 // discard attributes
    val UnpEnd   = P( "</xml:unparsed>" )
    val UnpData  = P( (!UnpEnd ~ AnyChar).rep )

    val CDSect  = P( CDStart ~/ CData.! ~ CDEnd ).map(p.PCData)
    val CDStart = P( "<![CDATA[" )
    val CData   = P( (!"]]>" ~ Char).rep )
    val CDEnd   = P( "]]>" )

    val Comment = P( "<!--" ~/ ComText.! ~ "-->" ).map(p.Comment)
    val ComText = P( (!"--" ~ Char).rep ~ ("-" ~ &("--")).? )

    val PI = P( "<?" ~ PITarget.! ~ PIProcText.? ~ "?>" ).map {
      case (target, text) => p.ProcInstr(target, text.getOrElse(""))
    }
    val PITarget   = P( !(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~ Name )
    val PIProcText = P( WL ~ (!"?>" ~ Char).rep.! )

    val Reference = P( EntityRef | CharRef )
    val EntityRef = P( "&" ~ Name.! ~/ ";" ).map(p.EntityRef)
    val CharRef   = P( "&#" ~ Num ~ ";" | "&#x" ~ HexNum ~ ";" ).map(c => p.Text(c.toString))
    val Num       = P( CharIn('0' to '9').rep.! ).map(n => p.charValueOf(n))
    val HexNum    = P( CharIn('0' to '9', 'a' to 'f', 'A' to 'F').rep.! ).map(n => p.charValueOf(n, 16))

    val CharData = P( Char1.rep(1).! ).map(p.Text)
    val Char = P( !Hole ~ AnyChar )
    val Char1 = P( !("<" | "&") ~ Char )
    val CharQ = P( !"\"" ~ Char1 )
    val CharA = P( !"'" ~ Char1 )

    val Name      = P( NameStart ~ NameChar.rep ).opaque("Name")
    val NameStart = P( CharPred(isNameStart) )
    val NameChar  = P( CharPred(isNameChar) )

    val ElemPattern: P[p.Node] = P( TagPHeader ~/ (EmptyElemTagEnd | ">" ~/ ContentP ~/ ETag ) ).map {
      case (qname, children: Seq[p.Node @unchecked]) => p.Node(qname, Nil, minimizeEmpty = false, children)
      case (qname, _)                                => p.Node(qname, Nil, minimizeEmpty = true, Nil)
    }
    val TagPHeader = P( "<" ~ Name.! ~ WL.?  )

    val ContentP      = P( (ScalaPatterns | ElemPattern | CharDataP ).rep )
    val CharDataP     = P( "&" ~ CharData.? | CharData ).!.map(p.Text) // matches weirdness of scalac parser on xml reference.
    val ScalaPatterns = ScalaExpr
  }
}
