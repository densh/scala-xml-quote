package scala.xml.quote

class UnquoteSuite extends XmlQuoteSuite {

  test("unquote within elem") {
    assert(xml"<foo>${2}</foo>" ≈ <foo>{2}</foo>)
    assert(xml"<foo>${"bar"}</foo>" ≈ <foo>{"bar"}</foo>)

    assert(xml"<foo>1</foo>" !≈ xml"<foo>${1}</foo>")

    assert(xml"<foo>${<bar/>}</foo>" ≈ <foo>{<bar/>}</foo>)
    assert(xml"<foo>${<bar/><bat/>}</foo>" ≈ <foo>{<bar/><bat/>}</foo>)
  }

  test("unquote within attribute") {
    assert(xml"<foo a=${"foo"}/>" ≈ <foo a={"foo"}/>)
    assert(xml"<foo a=${<bar/>}/>" ≈ <foo a={<bar/>}/>)
    assert(xml"<foo a=${<bar/><bat/>}/>" ≈ <foo a={<bar/><bat/>}/>)
  }
}
