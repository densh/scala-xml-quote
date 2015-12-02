package scala.xml

import scala.language.experimental.macros
import scala.xml.quote.internal.QuoteImpl

package object quote {
  implicit class XMLQuote(ctx: StringContext) {
    object xml {
      def apply[T](args: T*): scala.xml.Node = macro QuoteImpl.apply[T]
    }
  }
}
