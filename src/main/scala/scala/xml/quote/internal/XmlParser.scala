package scala.xml.quote
package internal

import fastparse.all._

import scala.xml.parsing.TokenTests
import scala.xml.quote.internal.QuoteImpl.Hole

// FIXME Name should not end by :
// FIXME tag must be balanced
class XmlParser(WL: P0 = CharsWhile(_.isWhitespace, 0)) extends TokenTests {
  import internal.{parsed => p}

  val ScalaExpr     = P( CharsWhile(Hole.isScalaExpr).! ).map(se => p.ScalaExpr(Hole.decode(se).get))
  val ScalaPatterns = ScalaExpr

  // TODO Maybe Xml.XmlContent.rep
  val XmlExpr: P[Seq[p.Node]] = P( WL ~ Xml.XmlContent ~ (WL ~ Xml.Element).rep ~ End ).map { case (n, ns) => n +: ns }
  val XmlPattern: P[p.Node]   = P( WL ~ Xml.ElemPattern )

  private[this] object Xml {

    val Element: P[p.Node] = P( TagHeader ~/ (EmptyElemTagEnd | ">" ~/ Content ~/ ETag ) ).map {
      case (qname, atts, children) => p.Node(qname, atts, children)
    }
    val TagHeader        = P( "<" ~ TagName.! ~/ (WL ~ Attribute).rep ~ WL.? )
    val TagName          = P( !"xml:unparsed" ~ Name )
    val EmptyElemTagEnd  = P( "/>" ).map(_ => Seq.empty[p.Node])
    val ETag             = P( "</" ~ TagName ~ WL.? ~ ">" )

    val Attribute = P( Name.! ~ Eq ~ AttValue ).map {
      case (qname, sc: p.ScalaExpr) => p.Attribute(qname, sc)
      case (qname, value: String)   => p.Attribute(qname, value)
    }
    val Eq       = P( WL.? ~ "=" ~ WL.? )
    val AttValue = P(
      "\"" ~/ (CharQ | Reference).rep.! ~ "\"" |
      "'" ~/ (CharA | Reference).rep.! ~ "'" |
      ScalaExpr
    )

    val Content               = P( (CharData | Reference | ScalaExpr | XmlContent).rep )
    val XmlContent: P[p.Node] = P( Element | CDSect | PI | Comment | Unparsed )

    val Unparsed = P( UnpStart ~/ UnpData.! ~ UnpEnd ).map(p.Unparsed)
    val UnpStart = P( "<xml:unparsed" ~ (WL ~ Attribute).rep ~ WL.? ~ ">" ).map(_ => Unit): P0 // discard attributes
    val UnpEnd   = P( "</xml:unparsed>" )
    val UnpData  = P( (!UnpEnd ~ AnyChar).rep )

    val CDSect  = P( CDStart ~/ CData.! ~ CDEnd ).map(p.PCData)
    val CDStart = P( "<![CDATA[" )
    val CData   = P( (!"]]>" ~ Char).rep )
    val CDEnd   = P( "]]>" )

    val Comment = P( "<!--" ~/ ((!"-" ~ Char) | ("-" ~ (!"-" ~ Char))).rep.! ~ "-->" ).map(p.Comment)

    val PI = P( "<?" ~ PITarget.! ~ PIProcText.?.! ~ "?>" ).map {
      case (target, text) => p.ProcInstr(target, text)
    }
    val PIProcText = P( WL ~ (!"?>" ~ Char).rep )
    val PITarget   = P( !(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~ Name )

    val Reference = P( EntityRef | CharRef )
    val EntityRef = P( "&" ~ Name.! ~/ ";" ).map(p.EntityRef)
    val CharRef   = P( "&#" ~ Num ~/ ";" | "&#x" ~ HexNum ~/ ";" ).map(c => p.Text(c.toString))
    val Num       = P( CharIn('0' to '9').rep.! ).map(n => p.charValueOf(n))
    val HexNum    = P( CharIn('0' to '9', 'a' to 'f', 'A' to 'F').rep.! ).map(n => p.charValueOf(n, 16))

    val CharData = P( Char1.rep(1).! ).map(p.Text)

    val Char  = P( CharPred(c => !Hole.isScalaExpr(c)) )
    val Char1 = P( !("<" | "&") ~ Char )
    val CharQ = P( !"\"" ~ Char1 )
    val CharA = P( !"'" ~ Char1 )

    val Name      = P( NameStart ~ NameChar.rep )
    val NameStart = P( CharPred(isNameStart) )
    val NameChar  = P( CharPred(isNameChar) )

    val ElemPattern: P[p.Node] = P( TagPHeader ~/ (EmptyElemTagEnd | ">" ~/ ContentP ~/ ETag ) ).map {
      case (qname, children) => p.Node(qname, Nil, children)
    }
    val TagPHeader = P( "<" ~ TagName.! ~ WL.?  )

    val ContentP  = P( (ElemPattern | CharDataP | ScalaPatterns).rep )
    val CharDataP = P( "&" ~ CharData.? | CharData ).!.map(p.Text) // matches weirdness of scalac parser on xml reference.
  }
}
