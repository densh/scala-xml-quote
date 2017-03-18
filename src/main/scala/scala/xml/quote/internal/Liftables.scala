package scala.xml.quote.internal

import scala.reflect.macros.whitebox.Context
import scala.xml.quote.internal.QuoteImpl.Hole

trait Liftables extends Nodes {
  val c: Context
  import c.universe._
  import c.universe.internal.reificationSupport.{SyntacticBlock => SynBlock}

  val sx = q"_root_.scala.xml"
  val sci = q"_root_.scala.collection.immutable"

  val args: List[Tree]

  lazy val argsIterator: Iterator[Tree] = args.iterator

  def NullableLiftable[T](f: T => Tree): Liftable[T] = Liftable { v =>
    if (v == null) q"null"
    else f(v)
  }

  implicit val liftComment: Liftable[xml.Comment] = Liftable { c =>
    q"new $sx.Comment(${c.commentText})"
  }

  implicit val liftText: Liftable[xml.Text] = Liftable { t =>
    q"new $sx.Text(${t.text})"
  }

  implicit val liftEntityRef: Liftable[xml.EntityRef] = Liftable { er =>
    q"new $sx.EntityRef(${er.entityName})"
  }

  implicit val liftUnquote: Liftable[Unquote] = Liftable { _.tree }

  implicit val liftProcInstr: Liftable[xml.ProcInstr] = Liftable { pi =>
    q"new $sx.ProcInstr(${pi.target}, ${pi.proctext})"
  }

  implicit val liftUnparsed: Liftable[xml.Unparsed] = Liftable { u =>
    q"new $sx.Unparsed(${u.data})"
  }

  implicit val liftPCData: Liftable[xml.PCData] = Liftable { pcd =>
    q"new $sx.PCData(${pcd.data})"
  }

  implicit def liftNamespaceBinding: Liftable[xml.NamespaceBinding] = NullableLiftable { ns =>
    if (ns eq xml.TopScope) q"$sx.TopScope"
    else q"new $sx.NamespaceBinding(${ns.prefix}, ${ns.uri}, ${ns.parent})"
  }

  implicit def liftElem(implicit outer: xml.NamespaceBinding = xml.TopScope): Liftable[xml.Elem] =
    Liftable { elem =>
      def liftMeta(meta: xml.MetaData): List[Tree] = meta match {
        case xml.Null =>
          q"var $$md: $sx.MetaData = $sx.Null" :: Nil
        case xml.UnprefixedAttribute(key, Seq(value), rest) =>
          q"$$md = new $sx.UnprefixedAttribute($key, $value, $$md)" :: liftMeta(rest)
        case xml.PrefixedAttribute(pre, key, Seq(value), rest) =>
          q"$$md = new $sx.PrefixedAttribute($pre, $key, $value, $$md)" :: liftMeta(rest)
      }

      val (metapre, metaval) =
        if (elem.attributes.isEmpty) (Nil, q"$sx.Null")
        else (liftMeta(elem.attributes).reverse, q"$$md")

      val children =
        if (elem.child.isEmpty) q""
        else {
          val outer = 'shadowed
          implicit val current: xml.NamespaceBinding = elem.scope
          val additions = elem.child.map { node => q"$$buf &+ $node" }
          q"""{
            val $$buf = new $sx.NodeBuffer
            ..$additions
            $$buf
          }: _*"""
        }

      def scoped(tree: Tree) = {
        def distinct(ns: xml.NamespaceBinding): List[(String, String)] =
          if (ns == null || ns.eq(outer) || ns.eq(xml.TopScope)) Nil
          else {
            val xml.NamespaceBinding(pre, uri, innerns) = ns
            (pre, uri) :: distinct(innerns)
          }

        val bindings = distinct(elem.scope)
        if (bindings.isEmpty) tree
        else {
          val q"..$stats" = tree
          val scopes = bindings.reverse.map { case (pre, uri) =>
            q"$$tmpscope = new $sx.NamespaceBinding($pre, $uri, $$tmpscope)"
          }
          q"""
            var $$tmpscope: $sx.NamespaceBinding = $$scope
            ..$scopes
            ${SynBlock(q"val $$scope: $sx.NamespaceBinding = $$tmpscope" :: stats)}
          """
        }
      }

    scoped(q"""
      ..$metapre
      new $sx.Elem(${elem.prefix}, ${elem.label}, $metaval, ${elem.scope},
                   ${elem.minimizeEmpty}, ..$children)
    """)
  }

  implicit val liftAtom: Liftable[xml.Atom[_]] = Liftable {
    case pcdata:   xml.PCData   => liftPCData(pcdata)
    case text:     xml.Text     => liftText(text)
    case unparsed: xml.Unparsed => liftUnparsed(unparsed)
    case hole:     Hole         => argsIterator.next
    case atom:     xml.Atom[_]  =>
      atom.data match {
        case c: Char   => q"new $sx.Atom($c)"
        case s: String => q"new $sx.Atom($s)"
        case v         => throw new Exception(s"unsupported atom value: $v (${v.getClass})")
      }
  }

  implicit val liftSpecialNode: Liftable[xml.SpecialNode] = Liftable {
    case atom:      xml.Atom[_]   => liftAtom(atom)
    case comment:   xml.Comment   => liftComment(comment)
    case procinstr: xml.ProcInstr => liftProcInstr(procinstr)
    case entityref: xml.EntityRef => liftEntityRef(entityref)
    case unquote:   Unquote       => liftUnquote(unquote)
  }

  implicit def liftGroup: Liftable[xml.Group] = Liftable {
    case xml.Group(nodes) =>
      q"new $sx.Group($sci.Seq(..$nodes))"
  }

  implicit def liftNode(implicit outer: xml.NamespaceBinding = xml.TopScope): Liftable[xml.Node] =
    Liftable {
      case elem:  xml.Elem        => liftElem(outer)(elem)
      case snode: xml.SpecialNode => liftSpecialNode(snode)
      case group: xml.Group       => liftGroup(group)
    }
}
