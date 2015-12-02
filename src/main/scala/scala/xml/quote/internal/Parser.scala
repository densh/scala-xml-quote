package scala.xml.quote
package internal

import scala.xml.parsing._

trait QuoteHandler extends ConstructingHandler

class QuoteParser(val input: io.Source, val preserveWS: Boolean)
  extends QuoteHandler
  with MarkupParser
  with ExternalSources
