package scala.xml.quote.internal

import scala.reflect.macros.whitebox
import scala.xml.quote.internal.parsed._

private[internal] trait Liftables {
  val c: whitebox.Context
  import c.universe._
  import Liftables.{Scope, TopScope}

  val args: List[Tree]

  private val sx = q"_root_.scala.xml"

  implicit def liftNode(implicit outer: Scope = TopScope): Liftable[Node] =
    Liftable {
      case n: Group       => liftGroup(outer)(n)
      case n: Elem        => liftElem(outer)(n)
      case n: Text        => liftText(n)
      case n: Placeholder => liftPlaceholder(n)
      case n: Comment     => liftComment(n)
      case n: PCData      => liftPCData(n)
      case n: ProcInstr   => liftProcInstr(n)
      case n: Unparsed    => liftUnparsed(n)
      case n: EntityRef   => liftEntityRef(n)
    }

  implicit def liftNodeSeq(implicit outer: Scope = TopScope): Liftable[Seq[Node]] = Liftable { nodes =>
    val additions = nodes.map(node => q"$$buf &+ $node")
    q"""
      {
        val $$buf = new $sx.NodeBuffer
        ..$additions
        $$buf
      }
    """
  }

  def liftGroup(implicit outer: Scope): Liftable[Group] = Liftable { gr =>
    q"new $sx.Group(${gr.nodes})"
  }

  def liftElem(implicit outer: Scope): Liftable[Elem] = Liftable { e =>
    def outerScope =
      if (outer.isTopScope) q"$sx.TopScope"
      else q"$$scope$$"

    def liftAttributes(atts: Seq[Attribute]): Seq[Tree] = {
      val metas = atts.reverse.map { a =>
        val value = a.value match {
          case Seq(v) => q"$v"
          case vs     => q"$vs"
        }
        a.pre match {
          case Some(prefix) =>
            q"$$md = new $sx.PrefixedAttribute($prefix, ${a.key}, $value, $$md)"
          case _ =>
            q"$$md = new $sx.UnprefixedAttribute( ${a.key}, $value, $$md)"
        }
      }

      val init: Tree = q"var $$md: $sx.MetaData = $sx.Null"
      init +: metas
    }

    def liftNameSpaces(nss: Seq[Attribute]): Seq[Tree] = {
      val init: Tree = q"var $$tmpscope: $sx.NamespaceBinding = $outerScope"

      val scopes = nss.map { ns =>
        val prefix = if (ns.pre.isDefined) q"${ns.key}" else q"null: String"
        val uri = ns.value.head match {
          case Text(text) => q"$text"
          case scalaExpr  => q"$scalaExpr"
        }
        q"$$tmpscope = new $sx.NamespaceBinding($prefix, $uri, $$tmpscope)"
      }

      init +: scopes
    }

    // wrong but like scalac: xmlnsfoo is a namespace
    def isNameSpace(a: Attribute) = a.name.startsWith("xmlns")
    val (nss, atts) = e.attributes.partition(isNameSpace)

    require {
      def hasValidURI(ns: Attribute) = ns.value match {
        case Seq(_: Text | _: Placeholder) => true
        case _                             => false
      }
      nss.forall(hasValidURI)
    }

    val prefix = e.prefix.fold(q"null: String": Tree)(p => q"$p")

    val (metapre, metaval) =
      if (atts.isEmpty) (Nil, q"$sx.Null")
      else (liftAttributes(atts), q"$$md")

    val _outer = outer
    val children = {
      implicit val outer: Scope = new Scope(_outer.isTopScope && nss.isEmpty)
      q"${e.children}"
    }

    if (nss.isEmpty) {
      q"""
        {
          ..$metapre
          new $sx.Elem($prefix, ${e.label}, $metaval, $outerScope, ${e.minimizeEmpty}, $children: _*)
        }
       """
    } else {
      val scopepre = liftNameSpaces(nss)
      q"""
        {
          ..$scopepre;
          {
            val $$scope$$ = $$tmpscope
            ..$metapre
            new $sx.Elem($prefix, ${e.label}, $metaval, $$scope$$, ${e.minimizeEmpty}, $children: _*)
          }
        }
       """
    }
  }

  val liftText: Liftable[Text] = Liftable { t =>
    q"new $sx.Text(${t.text})"
  }

  val liftPlaceholder: Liftable[Placeholder] = Liftable { se =>
    args(se.id)
  }

  val liftComment: Liftable[Comment] = Liftable { c =>
    q"new $sx.Comment(${c.commentText})"
  }

  val liftPCData: Liftable[PCData] = Liftable { pcd =>
    q"new $sx.PCData(${pcd.data})"
  }

  val liftProcInstr: Liftable[ProcInstr] = Liftable { pi =>
    q"new $sx.ProcInstr(${pi.target}, ${pi.proctext})"
  }

  val liftUnparsed: Liftable[Unparsed] = Liftable { u =>
    q"new $sx.Unparsed(${u.data})"
  }

  val liftEntityRef: Liftable[EntityRef] = Liftable { er =>
    q"new $sx.EntityRef(${er.entityName})"
  }
}

private[internal] object Liftables {
  class Scope(val isTopScope: Boolean) extends AnyVal
  final val TopScope = new Scope(true)
}
