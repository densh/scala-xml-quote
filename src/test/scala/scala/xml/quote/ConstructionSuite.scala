package scala.xml.quote

import org.scalatest.FunSuite

class ConstructionSuite extends FunSuite {
  test("reconstruct comment") {
    assert(xml"<!--foo-->" == <!--foo-->)
  }

  test("reconstruct group") {
    assert(xml"<xml:group><foo/><bar/></xml:group>" == <xml:group><foo/><bar/></xml:group>)
  }

  test("reconstruct text") {
    assert(xml"<![CDATA[foo]]>" == <![CDATA[foo]]>)
  }

  test("reconstruct entity ref") {
    assert(xml"<foo>&amp;</foo>" == <foo>&amp;</foo>)
  }

  test("reconstruct proc instr") {
    assert(xml"<foo><?foo bar?></foo>" == <foo><?foo bar?></foo>)
  }

  test("reconstruct unparsed") {
    assert(xml"<xml:unparsed>foo</xml:unparsed>" == <xml:unparsed>foo</xml:unparsed>)
  }

  test("reconstruct minimized elem") {
    assert(xml"<foo/>" == <foo/>)
  }

  test("reconstruct maximized elem") {
    assert(xml"<foo></foo>" == <foo></foo>)
  }

  test("reconstruct prefixed elem") {
    assert(xml"<foo:bar/>" == <foo:bar/>)
  }

  test("reconstruct nested elem") {
    assert(xml"<foo><bar/></foo>" == <foo><bar/></foo>)
  }

  test("reconstruct elem with unprefixed attributes") {
    assert(xml"""<foo a="a" b="b"/>""" == <foo a="a" b="b"/>)
  }

  test("reconstruct elem with prefixed attributes") {
    assert(xml"""<foo a:a="a" b:b="b"/>""" == <foo a:a="a" b:b="b"/>)
  }

  test("reconstruct namespaced elem") {
    assert(xml"""<foo xmlns:pre="uri"/>""" == <foo xmlns:pre="uri"/>)
  }

  test("reconstruct multi-namespaced elem") {
    assert(xml"""<foo xmlns:a="uri1" xmlns:b="uri2"/>""" == <foo xmlns:a="uri1" xmlns:b="uri2"/>)
  }

  test("reconstruct nested namespaced elem") {
    assert(xml"""<foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>""" == <foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>)
  }

  test("reconstruct shadowed namespaced elem") {
    assert(xml"""<foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>""" == <foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>)
  }

   test("reconstruct unquote within elem") {
     assert(xml"<foo>${2 + 3}</foo>" == <foo>{2 + 3}</foo>)
   }

  // test("reconstruct unquote within unprefixed attribute") {
  //   assert(xml"<foo a=${"foo" + "bar"}/>" == <foo a={"foo" + "bar"}/>)
  // }

  // test("reconstruct unquote within prefixed attribute") {
  //   assert(xml"<foo a:b=${"foo" + "bar"}/>" == <foo a:b={"foo" + "bar"}/>)
  // }
}
