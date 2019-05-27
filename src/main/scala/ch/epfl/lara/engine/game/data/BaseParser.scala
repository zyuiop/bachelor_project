package ch.epfl.lara.engine.game.data

import ch.epfl.lara.engine.game.items.Pickable

import scala.util.parsing.combinator.RegexParsers

/**
  * @author Louis Vialar
  */
class BaseParser extends RegexParsers {
  def identifier: Parser[String] = {
    def simpleIdentifier = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

    def anyIdentifier = "[a-zA-Z0-9_]*".r ^^ { str => str }

    def spacedIdentifier = """`[^`]*`""".r ^^ { str => str drop 1 dropRight 1 }

    chainl1(simpleIdentifier | spacedIdentifier, spacedIdentifier | anyIdentifier, "." ^^^ { (s1: String, s2: String) => s1 + "." + s2 } )
  }

  def shortStringLiteral: Parser[String] = """"[^"]*"""".r ^^ { str => str drop 1 dropRight 1 }

  def longStringLiteral: Parser[String] = "\"\"\".*\"\"\"".r ^^ { str => str drop 3 dropRight 3 replace("\\n", "\n") replace("\\t", "\t") }

  def stringLiteral = longStringLiteral ||| shortStringLiteral

  def intLiteral: Parser[String] = """[0-9]+""".r ^^ (v => v)

  def keyValue = identifier ~ "=" ~! (stringLiteral | intLiteral) ^^ { case l ~ eq ~ r => (l, r) }

  def properties = keyValue.+ ^^ { l => l.toMap }
}
