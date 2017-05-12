package scala.xml

import scala.language.experimental.macros
import scala.xml.quote.internal.QuoteImpl

package object quote {
  implicit class XmlQuote(ctx: StringContext) {
    object xml {
      def apply[T](args: T*): AnyRef = macro QuoteImpl.apply[T]
    }
  }
}
