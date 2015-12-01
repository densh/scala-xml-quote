import scala.Predef.{any2stringadd => _, _}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import org.scalamacros.xml.MacroLiftables
import java.util.UUID.randomUUID

package object xmlquote {
  implicit class XMLQuote(ctx: StringContext) {
    object xml {
      def apply[T](args: T*): scala.xml.Node = macro Impl.apply[T]
    }
  }
}

package xmlquote {
  private[xmlquote] class Impl(val c: Context) extends MacroLiftables { import c.universe._
    lazy val sessionSuffix = randomUUID().toString.replace("-", "").substring(0, 8)
    lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication
    lazy val (xmlstr, subsMap) = {
      val sb = new StringBuilder
      var subsMap = Map.empty[String, Tree]
      var i = 0
      args.zip(parts.init).foreach { case (arg, part) =>
        sb.append(part)
        val key1 = sessionSuffix + i
        i += 1
        val key2 = "\"" + key1 + "\""
        sb.append(key2)
        subsMap += key1 -> arg
        subsMap += key2 -> arg
      }
      sb.append(parts.last)
      (sb.toString, subsMap)
    }
    def transformText(text: xml.Text) = {
      val s = text.text
      val regex = "(?<=\"" + sessionSuffix + "\\d{1,3}\")|(?=\"" + sessionSuffix + "\\d{1,3}\")"
      val splitted = s.split(regex).toList
      val subs = splitted.collect {
        case part if subsMap contains part => Unquote(subsMap(part))
        case part if part.nonEmpty         => xml.Text(part)
      }
      if (subs.length == 1) subs.head
      else subs
    }
    def transformMetaData(md: xml.MetaData): xml.MetaData = md match {
      case xml.UnprefixedAttribute(name, value: xml.Text, rest) =>
        new xml.UnprefixedAttribute(name, transformText(value), transformMetaData(rest))
      case xml.PrefixedAttribute(pre, name, value: xml.Text, rest) =>
        new xml.PrefixedAttribute(pre, name, transformText(value), transformMetaData(rest))
      case xml.Null => xml.Null
    }
    def transformNode(n: xml.Node): Seq[xml.Node] = n match {
      case text: xml.Text if text.text.contains(sessionSuffix) =>
        transformText(text)
      case elem: xml.Elem =>
        elem.copy(attributes = transformMetaData(elem.attributes),
                  child = elem.child.flatMap(transformNode))
      case  _ => n
    }
    def apply[T](args: Tree*): Tree = {
      val parsed =
        try xml.XML.loadString(xmlstr)
        catch {
          case exc: org.xml.sax.SAXParseException =>
            c.abort(c.macroApplication.pos, exc.getMessage)
        }
      val transformed = transformNode(parsed).head
      q"{ val $$scope = _root_.scala.xml.TopScope; $transformed }"
    }
  }
}
