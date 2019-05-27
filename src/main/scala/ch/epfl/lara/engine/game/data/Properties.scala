package ch.epfl.lara.engine.game.data

import ch.epfl.lara.engine.game.items.Pickable

/**
  * @author Louis Vialar
  */
object Properties {
  implicit class PropertiesMap(map: Map[String, String]) {
    /**
      * Extract a subset of this map of all the keys prefixed with a given string from a map,
      * removing the prefix from the key
      *
      * @param prefix the prefix to look for
      * @return a map, containing only the `k`->`v` pairs for which `prefix`.`k` is a key of `map`
      */
    def prefixed(prefix: String): Map[String, String] = {
      val len = prefix.length + 1
      map.filter(_._1.startsWith(prefix + ".")).map(pair => pair._1.drop(len) -> pair._2)
    }

    def inventory(prefix: String): Map[Pickable, Int] =
      prefixed(prefix).map(pair => Pickable(pair._1) -> pair._2.toInt)

    /**
      * Extract all the values whose keys prefixed with a given string from a map, removing the prefix from the key
      *
      * @param prefix the prefix to look for
      * @return a list, containing only the strings for which the key s verifies `prefix`.`s` is a key of `map`
      */
    def multiVal(prefix: String): List[String] =
      prefixed(prefix).values.toList
  }
}
