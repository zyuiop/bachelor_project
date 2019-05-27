package ch.epfl.lara.engine.api.data

import ch.epfl.lara.engine.game.items.Item

/**
  * @author Louis Vialar
  */
trait LevelParser {
  def registerItemType(name: String)(builder: Map[String, String] => Item): LevelParser
}
