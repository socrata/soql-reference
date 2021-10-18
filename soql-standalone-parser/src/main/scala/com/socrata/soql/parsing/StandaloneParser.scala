package com.socrata.soql.parsing

import com.socrata.soql.parsing.standalone_exceptions.BadParse
import scala.util.parsing.input.Position
import java.io.StringReader

class StandaloneParser(parameters: AbstractParser.Parameters = AbstractParser.defaultParameters) extends RecursiveDescentParser(parameters) {
  protected def expected(reader: RecursiveDescentParser.Reader) =
    new BadParse.ExpectedToken(reader)

  protected def expectedLeafQuery(reader: RecursiveDescentParser.Reader) =
    new BadParse.ExpectedLeafQuery(reader)

  protected def lexer(s: String): AbstractLexer = new StandaloneLexer(s)
}

class StandaloneCombinatorParser(parameters: AbstractParser.Parameters = AbstractParser.defaultParameters) extends AbstractCombinatorParser(parameters) {
  protected def badParse(msg: String, nextPos: Position): Nothing =
    throw BadParse(msg, nextPos)

  protected def lexer(s: String): AbstractLexer = new StandaloneLexer(s)
}
