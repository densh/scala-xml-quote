import org.scalatest.FunSuite
import scala.xml.quote._

class ConstructionSuite extends FunSuite {
  import ConstructionSuite._

  test("reproduce scalac weirdness and bugs") {
    // empty CharRef
    assert(xml"""<a b="&#;"/>""" ≡≡ <a b="&#;"/>)
    assert(xml"""<a b="&#x;"/>""" ≡≡ <a b="&#x;"/>)
    assert(xml"""<a>&#;</a>""" ≡≡ <a>&#;</a>)
    assert(xml"""<a>&#x;</a>""" ≡≡ <a>&#x;</a>)

    // closing PCData
    assert(xml"""<a>]]></a> """ ≡≡ <a>]]></a> )

    // weird namespace
    //FIXME assert(xml"""<a xmlnshello="scope1"/>""" ≡≡ <a xmlnshello="scope1"/>)

  }

  test("reconstruct comment") {
    assert(xml"<!--foo-->" ≡≡ <!--foo-->)
  }

  test("reconstruct sequence of nodes") {
    assert(xmls"<foo/><bar/>" ≡≡ <foo/><bar/>)
  }

  test("reconstruct group") {
    assert(xml"<xml:group><foo/><bar/></xml:group>" ≡≡ <xml:group><foo/><bar/></xml:group>)
  }

  test("reconstruct text") {
    assert(xml"<![CDATA[foo]]>" ≡≡ <![CDATA[foo]]>)
  }

  test("reconstruct entity ref") {
    assert(xml"<foo>&amp;</foo>" ≡≡ <foo>&amp;</foo>)
  }

  test("reconstruct proc instr") {
    assert(xml"<foo><?foo bar?></foo>" ≡≡ <foo><?foo bar?></foo>)
  }

  test("reconstruct unparsed") {
    assert(xml"<xml:unparsed><</xml:unparsed>" ≡≡ <xml:unparsed><</xml:unparsed>)
  }

  test("reconstruct minimized elem") {
    assert(xml"<foo/>" ≡≡ <foo/>)
  }

  test("reconstruct maximized elem") {
    assert(xml"<foo></foo>" ≡≡ <foo></foo>)
  }

  test("reconstruct prefixed elem") {
    assert(xml"<foo:bar/>" ≡≡ <foo:bar/>)
  }

  test("reconstruct nested elem") {
    assert(xml"<foo><bar/></foo>" ≡≡ <foo><bar/></foo>)
  }

  test("reconstruct elem with unprefixed attributes") {
    assert(xml"""<foo a="a" b="b"/>""" ≡≡ <foo a="a" b="b"/>)
  }

  test("reconstruct elem with prefixed attributes") {
    assert(xml"""<foo a:a="a" b:b="b"/>""" ≡≡ <foo a:a="a" b:b="b"/>)
  }

  test("reconstruct namespaced elem") {
    assert(xml"""<foo xmlns:pre="uri"/>""" ≡≡ <foo xmlns:pre="uri"/>)
  }

  test("reconstruct multi-namespaced elem") {
    assert(xml"""<foo xmlns:a="uri1" xmlns:b="uri2"/>""" ≡≡ <foo xmlns:a="uri1" xmlns:b="uri2"/>)
  }

  test("reconstruct nested namespaced elem") {
    assert(xml"""<foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>""" ≡≡ <foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>)
  }

  test("reconstruct shadowed namespaced elem") {
    assert(xml"""<foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>""" ≡≡ <foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>)
  }

  test("reconstruct unquote within elem") {
   assert(xml"<foo>${2 + 3}</foo>" ≡≡ <foo>{2 + 3}</foo>)
  }

  test("reconstruct unquote within unprefixed attribute") {
   assert(xml"<foo a=${"foo" + "bar"}/>" ≡≡ <foo a={"foo" + "bar"}/>)
  }

  test("reconstruct unquote within prefixed attribute") {
   assert(xml"<foo a:b=${"foo" + "bar"}/>" ≡≡ <foo a:b={"foo" + "bar"}/>)
  }

  test("reconstruct unquote within namespaced elem") {
    assert(xml"""<foo xmlns:pre=${"foo" + "bar"}/>""" ≡≡ <foo xmlns:pre={"foo" + "bar"}/>)
  }

  test("reconstruct nested interpolator") {
    val xml1 =
      xml"""<a xmlns:pre="scope0">${ xml"<b/>" }</a>"""

    val xml2 = <a xmlns:pre="scope0">{ <b/> }</a>
    //FIXME assert(xml1 ≡≡ xml2)
  }

  test("reconstruct multiline element") {
    val xml1 = xml"""
      <a>
        <b/>
      </a>
    """

    val xml2 =
      <a>
        <b/>
      </a>

    assert(xml1 ≡≡ xml2)
  }
}

object ConstructionSuite {

  implicit class NodeOps(val self: xml.Node) extends AnyVal {
    /** `==` + scope comparison */
    def ≡≡(that: xml.Node): Boolean =
      self == that && hasSameScope(self, that)

    private def hasSameScope(self: xml.Node, that: xml.Node): Boolean =
      self.scope == that.scope && {
        val zipped = (self, that) match {
          case (g1: xml.Group, g2: xml.Group) => (g1.nodes, g2.nodes).zipped
          case (n1, n2)                       => (n1.child, n2.child).zipped
        }
        zipped.forall(hasSameScope)
      }
  }

  implicit class NodeBufferOps(val self: xml.NodeBuffer) extends AnyVal {
    /** `==` + scope comparison */
    def ≡≡(that: xml.NodeBuffer): Boolean = {
      val selfIt = self.iterator
      val thatIt = that.iterator

      while (selfIt.hasNext && thatIt.hasNext) {
        if (!(selfIt.next() ≡≡ thatIt.next())) {
          return false
        }
      }

      !selfIt.hasNext && !thatIt.hasNext
    }
  }
}
