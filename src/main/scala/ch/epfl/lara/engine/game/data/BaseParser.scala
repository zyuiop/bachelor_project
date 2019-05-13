package ch.epfl.lara.engine.game.data

import ch.epfl.lara.engine.game.data.CharacterParser.prefixed
import ch.epfl.lara.engine.game.items.Pickable

import scala.util.parsing.combinator.RegexParsers

/**
  * @author Louis Vialar
  */
class BaseParser extends RegexParsers {
  def simpleIdentifier: Parser[String] = "[a-zA-Z_][a-zA-Z0-9._]*".r

  def stringLiteral: Parser[String] = """"[^"]*"""".r ^^ { str => str drop 1 dropRight 1 }

  def intLiteral: Parser[String] = """[0-9]+""".r ^^ (v => v)

  def keyValue = simpleIdentifier ~ "=" ~! (stringLiteral | intLiteral) ^^ { case l ~ eq ~ r => (l, r) }

  def properties = keyValue.+ ^^ { l => l.toMap }

  /**
    * Extract a subset of this map of all the keys prefixed with a given string from a map,
    * removing the prefix from the key
    * @param prefix the prefix to look for
    * @param map the map in which the search will be ran
    * @return a map, containing only the `k`->`v` pairs for which `prefix`.`k` is a key of `map`
    */
  def prefixed(prefix: String, map: Map[String, String]): Map[String, String] = {
    val len = prefix.length + 1
    map.filter(_._1.startsWith(prefix + ".")).map(pair => pair._1.drop(len) -> pair._2)
  }

  def inventory(prefix: String, map: Map[String, String]): Map[Pickable, Int] =
    prefixed(prefix, map).map(pair => Pickable(pair._1) -> pair._2.toInt)

  /**
    * Extract all the values whose keys prefixed with a given string from a map, removing the prefix from the key
    * @param prefix the prefix to look for
    * @param map the map in which the search will be ran
    * @return a list, containing only the strings for which the key s verifies `prefix`.`s` is a key of `map`
    */
  def multiVal(prefix: String, map: Map[String, String]): List[String] =
    prefixed(prefix, map).values.toList



}
