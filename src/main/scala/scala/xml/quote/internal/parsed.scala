package scala.xml.quote.internal

import scala.collection.mutable.ListBuffer

private[internal] object parsed {

  sealed trait Node

  object Node {
    def apply(qname: String,
              attributes: Seq[Attribute],
              minimizeEmpty: Boolean,
              children: Seq[Node]): Node = {

      val duplicates = attributes
        .groupBy(a => a.pre.fold(a.key)(pre => s"$pre:${a.key}"))
        .collect { case (key, as) if as.size > 1 => key }
      require(duplicates.isEmpty, s"attribute(s) ${duplicates.mkString(", ")} may only be defined once")

      val child =
        if (XmlSettings.isCoalescing) coalesce(children)
        else children

      if (qname == "xml:group" && !minimizeEmpty) { // <xml:group/> is Elem in scalac
        Group(child)
      } else {
        val (prefix, label) = prefixAndName(qname)
        Elem(prefix, label, attributes, minimizeEmpty, child)
      }
    }

    /** Merge text sections */
    private def coalesce(nodes: Seq[Node]): Seq[Node] = {
      val buf = new ListBuffer[Node]
      val sb = new StringBuilder

      def purgeText() = {
        if (sb.nonEmpty) {
          buf += Text(sb.result())
          sb.clear()
        }
      }

      nodes.foreach {
        case Text(text)   => sb ++= text
        case PCData(data) => sb ++= data
        case n =>
          purgeText()
          buf += n
      }

      purgeText()
      buf.toList
    }
  }

  /** <xml:group><node1/><node2/></xml:group> */
  final case class Group(nodes: Seq[Node]) extends Node

  /** <prefix:label attributeKey:"attributeValue">
    *   <child1/>
    *   <child2/>
    * </prefix:label>
    */
  final case class Elem(prefix: Option[String],
                        label: String,
                        attributes: Seq[Attribute],
                        minimizeEmpty: Boolean,
                        children: Seq[Node]) extends Node

  /** <foo>text</foo> */
  final case class Text(text: String) extends Node

  final case class Placeholder(id: Int) extends Node

  /** <!--commentText--> */
  final case class Comment(commentText: String) extends Node

  /** <![CDATA[data]]> */
  final case class PCData(data: String) extends Node

  /** <?foo bar?> */
  final case class ProcInstr(target: String, proctext: String) extends Node

  /** <xml:unparsed>data</xml:unparsed> */
  final case class Unparsed(data: String) extends Node

  /** <foo>&entityName;</foo> */
  final case class EntityRef(entityName: String) extends Node

  /** <foo pre:key="value" />
    *
    * @param value is `Text` or `{scalaExpression}`
    */
  final case class Attribute(pre: Option[String], key: String, value: Seq[Node]) {
    def name: String = pre.fold(key)(p => s"$p:$key")
  }

  object Attribute {

    def apply(qname: String, value: String): Attribute = {
      val (pre, key) = prefixAndName(qname)
      Attribute(pre, key, normalizeAttValue(value))
    }

    def apply(qname: String, value: Placeholder): Attribute = {
      val (pre, key) = prefixAndName(qname)
      Attribute(pre, key, Seq(value))
    }

    /** Replaces character and entity references */
    private def normalizeAttValue(value: String): Seq[Node] = {
      def ref(it : Iterator[Char]) = it.takeWhile(_ != ';').mkString

      val it = value.iterator.buffered
      val buf = new ListBuffer[Node]
      val sb = new StringBuilder

      def purgeText() = {
        if (sb.nonEmpty) {
          buf += Text(sb.result())
          sb.clear()
        }
      }

      while (it.hasNext) it.next() match {
        case ' ' | '\t' | '\n' | '\r' =>
          sb += ' '

        case '&' if it.head == '#' =>
          it.next()
          val radix =
            if (it.head == 'x') { it.next(); 16 }
            else 10
          sb += charValueOf(ref(it), radix)

        case '&' =>
          val name = ref(it)
          attrUnescape.get(name) match {
            case Some(c) =>
              sb += c
            case _ =>
              purgeText()
              buf += EntityRef(name)
          }

        case c =>
          sb += c

      }

      purgeText()
      buf.result()
    }

    private val attrUnescape = Map(
      "lt"    -> '<',
      "gt"    -> '>',
      "apos"  -> '\'',
      "quot"  -> '"',
      "quote" -> '"'
    )
  }

  /** Extract prefix from `qname`
    *
    * @param qname
    * @return `prefix`if present and the name after the prefix
    */
  private def prefixAndName(qname: String) = qname.indexOf(":") match {
    case -1 =>
      (None, qname)
    case i =>
      val prefix = qname.substring(0, i)
      val key = qname.substring(i + 1, qname.length)
      (Some(prefix), key)
  }

  def charValueOf(cr: String, radix: Int = 10): Char =
    if (cr.isEmpty) 0.toChar
    else java.lang.Integer.parseInt(cr, radix).toChar
}
