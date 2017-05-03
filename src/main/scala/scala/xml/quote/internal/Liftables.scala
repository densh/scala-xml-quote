package scala.xml.quote.internal

import scala.reflect.macros.blackbox
import scala.xml.quote.internal.{parsed => p}

trait Liftables {
  val c: blackbox.Context
  import c.universe._
  import Liftables.{Scope, TopScope}

  val args: List[Tree]

  private val sx = q"_root_.scala.xml"

  implicit def liftNode(implicit outer: Scope = TopScope): Liftable[p.Node] =
    Liftable {
      case n: p.Group     => liftGroup(outer)(n)
      case n: p.Elem      => liftElem(outer)(n)
      case n: p.Text      => liftText(n)
      case n: p.ScalaExpr => liftScalaExp(n)
      case n: p.Comment   => liftComment(n)
      case n: p.PCData    => liftPCData(n)
      case n: p.ProcInstr => liftProcInstr(n)
      case n: p.Unparsed  => liftUnparsed(n)
      case n: p.EntityRef => liftEntityRef(n)
    }

  implicit def liftNodeSeq(implicit outer: Scope = TopScope): Liftable[Seq[p.Node]] = Liftable { nodes =>
    val additions = nodes.map(node => q"$$buf &+ $node")
    q"""
      {
        val $$buf = new $sx.NodeBuffer
        ..$additions
        $$buf
      }
    """
  }

  def liftGroup(implicit outer: Scope): Liftable[p.Group] = Liftable { gr =>
    q"new $sx.Group(${gr.nodes})"
  }

  def liftElem(implicit outer: Scope): Liftable[p.Elem] = Liftable { e =>
    def outerScope =
      if (outer.isTopScope) q"$sx.TopScope"
      else q"$$scope$$"

    def liftAttributes(atts: Seq[p.Attribute]): Seq[Tree] = {
      val init: Tree = q"var $$md: $sx.MetaData = $sx.Null"

      val metas = atts.reverse.map { a =>
        val prefix = a.pre.fold(q"null: String": Tree)(p => q"$p")
        val value = a.value match {
          case Seq(v) => q"$v"
          case vs     => q"$vs"
        }
        q"$$md = $sx.Attribute($prefix, ${a.key}, $value, $$md)"
      }

      init +: metas
    }

    def liftNameSpaces(nss: Seq[p.Attribute]): Seq[Tree] = {
      val init: Tree = q"var $$tmpscope: $sx.NamespaceBinding = $outerScope"

      val scopes = nss.map { ns =>
        val prefix = if (ns.pre.isDefined) q"${ns.key}" else q"null: String"
        val uri = ns.value.head match {
          case p.Text(text) => q"$text"
          case scalaExpr    => q"$scalaExpr"
        }
        q"$$tmpscope = $sx.NamespaceBinding($prefix, $uri, $$tmpscope)"
      }

      init +: scopes
    }

    // wrong but like scalac: xmlnsfoo is a namespace
    def isNameSpace(a: p.Attribute) = a.name.startsWith("xmlns")
    val (nss, atts) = e.attributes.partition(isNameSpace)

    require {
      def hasValidURI(ns: p.Attribute) = ns.value match {
        case Seq(_: p.Text | _: p.ScalaExpr) => true
        case _                               => false
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
          $sx.Elem($prefix, ${e.label}, $metaval, $outerScope, ${e.minimizeEmpty}, $children: _*)
        }
       """
    } else {
      val scopepre = liftNameSpaces(nss)
      q"""
        {
          ..$scopepre

          {
            val $$scope$$ = $$tmpscope
            ..$metapre
            $sx.Elem($prefix, ${e.label}, $metaval, $$scope$$, ${e.minimizeEmpty}, $children: _*)
          }
        }
       """
    }
  }

  val liftText: Liftable[p.Text] = Liftable { t =>
    q"new $sx.Text(${t.text})"
  }

  val liftScalaExp: Liftable[p.ScalaExpr] = Liftable { se =>
    args(se.id)
  }

  val liftComment: Liftable[p.Comment] = Liftable { c =>
    q"new $sx.Comment(${c.commentText})"
  }

  val liftPCData: Liftable[p.PCData] = Liftable { pcd =>
    q"new $sx.PCData(${pcd.data})"
  }

  val liftProcInstr: Liftable[p.ProcInstr] = Liftable { pi =>
    q"new $sx.ProcInstr(${pi.target}, ${pi.proctext})"
  }

  val liftUnparsed: Liftable[p.Unparsed] = Liftable { u =>
    q"new $sx.Unparsed(${u.data})"
  }

  val liftEntityRef: Liftable[p.EntityRef] = Liftable { er =>
    q"new $sx.EntityRef(${er.entityName})"
  }
}

private[internal] object Liftables {
  class Scope(val isTopScope: Boolean) extends AnyVal
  final val TopScope = new Scope(true)
}
