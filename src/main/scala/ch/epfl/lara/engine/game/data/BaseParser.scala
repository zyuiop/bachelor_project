package ch.epfl.lara.engine.game.data

import ch.epfl.lara.engine.api.data.Properties
import ch.epfl.lara.engine.game.items.Pickable

import scala.util.parsing.combinator.RegexParsers

/**
  * @author Louis Vialar
  */
class BaseParser extends RegexParsers with Properties {
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

  /**
    * Extract a subset of this map of all the keys prefixed with a given string from a map,
    * removing the prefix from the key
    *
    * @param prefix the prefix to look for
    * @param map    the map in which the search will be ran
    * @return a map, containing only the `k`->`v` pairs for which `prefix`.`k` is a key of `map`
    */
  def prefixed(prefix: String, map: Map[String, String]): Map[String, String] = map.prefixed(prefix)

  def inventory(prefix: String, map: Map[String, String]): Map[Pickable, Int] = map.inventory(prefix)

  /**
    * Extract all the values whose keys prefixed with a given string from a map, removing the prefix from the key
    *
    * @param prefix the prefix to look for
    * @param map    the map in which the search will be ran
    * @return a list, containing only the strings for which the key s verifies `prefix`.`s` is a key of `map`
    */
  def multiVal(prefix: String, map: Map[String, String]): List[String] = map.multiVal(prefix)


}
